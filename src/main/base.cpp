/* The beginning of code which represents an R base graphics system
 * separate from an R graphics engine (separate from R devices)
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Graphics.h>
#include <Rdevices.h>

int attribute_hidden baseRegisterIndex = -1;

void restoredpSaved(DevDesc *dd);

static SEXP baseCallback(GEevent task, GEDevDesc *dd, SEXP data) {
    GEDevDesc *curdd;
    GESystemDesc *sd;
    NewDevDesc *dev;
    GPar *ddp;
    GPar *ddpSaved;
    SEXP state;
    SEXP valid;
    SEXP result = R_NilValue;
    switch (task) {
    case GE_FinaliseState:
	sd = dd->gesd[baseRegisterIndex];
	free(sd->systemSpecific);
	sd->systemSpecific = NULL;
	break;
    case GE_InitState:
	sd = dd->gesd[baseRegisterIndex];
	dev = dd->dev;
	sd->systemSpecific = malloc(sizeof(baseSystemState));
	ddp = &(reinterpret_cast<baseSystemState*>(sd->systemSpecific)->dp);
	GInit(ddp);
	/* Some things are set by the device, so copy them across now.
	 */
	ddp->ipr[0] = dev->ipr[0];
	ddp->ipr[1] = dev->ipr[1];
	ddp->cra[0] = dev->cra[0];
	ddp->cra[1] = dev->cra[1];
	ddp->asp = dev->asp;
	ddp->left = dev->left;
	ddp->right = dev->right;
	ddp->top = dev->top;
	ddp->bottom = dev->bottom;
	ddp->xCharOffset = dev->xCharOffset;
	ddp->yCharOffset = dev->yCharOffset;
	ddp->yLineBias = dev->yLineBias;
	ddp->canResizePlot = dev->canResizePlot;
	ddp->canChangeFont = dev->canChangeFont;
	ddp->canRotateText = dev->canRotateText;
	ddp->canResizeText = dev->canResizeText;
	ddp->canClip = dev->canClip;
	ddp->canHAdj = dev->canHAdj;
	/* For some things, the device sets the starting value at least.
	 */
	ddp->ps = int(dev->startps);
	ddp->col = ddp->fg = dev->startcol;
	ddp->bg = dev->startfill;
	ddp->font = dev->startfont; 
	ddp->lty = dev->startlty; 
	ddp->gamma = dev->startgamma;
	/* Initialise the gp settings too.
	 */
	/* copyGPar(ddp, &(((baseSystemState*) sd->systemSpecific)->gp)); */
	/*
	 * The device has not yet received any base output
	 */
	reinterpret_cast<baseSystemState*>(sd->systemSpecific)->baseDevice = FALSE;
	break;
    case GE_CopyState:
	sd = dd->gesd[baseRegisterIndex];
	curdd = GEcurrentDevice();
	copyGPar(&(reinterpret_cast<baseSystemState*>(sd->systemSpecific)->dpSaved),
		 &(reinterpret_cast<baseSystemState*> 
		   (curdd->gesd[baseRegisterIndex]->systemSpecific)->dpSaved));
	restoredpSaved(reinterpret_cast<DevDesc*>(curdd));
	copyGPar(&(reinterpret_cast<baseSystemState*>
		   (curdd->gesd[baseRegisterIndex]->systemSpecific)->dp),
		 &(reinterpret_cast<baseSystemState*>
		   (curdd->gesd[baseRegisterIndex]->systemSpecific)->gp));
	GReset(reinterpret_cast<DevDesc*>(curdd));
	break;
    case GE_SaveState:
	sd = dd->gesd[baseRegisterIndex];
	copyGPar(&(reinterpret_cast<baseSystemState*>(sd->systemSpecific)->dp),
		 &(reinterpret_cast<baseSystemState*>(sd->systemSpecific)->dpSaved));
	break;
    case GE_RestoreState:
	sd = dd->gesd[baseRegisterIndex];
	restoredpSaved(reinterpret_cast<DevDesc*>(dd));
	copyGPar(&(reinterpret_cast<baseSystemState*>(sd->systemSpecific)->dp),
		 &(reinterpret_cast<baseSystemState*>(sd->systemSpecific)->gp));
	GReset(reinterpret_cast<DevDesc*>(dd));
	break;
    case GE_SaveSnapshotState:
	sd = dd->gesd[baseRegisterIndex];
	PROTECT(state = allocVector(INTSXP,
				    /* Got this formula from devga.c
				     * Not sure why the "+ 1"
				     * Rounding up?
				     */
				    1 + sizeof(GPar) / sizeof(int)));
	copyGPar(&(reinterpret_cast<baseSystemState*>(sd->systemSpecific)->dpSaved),
		 reinterpret_cast<GPar*>(INTEGER(state)));
	result = state;
	UNPROTECT(1);
	break;
    case GE_RestoreSnapshotState:
	sd = dd->gesd[baseRegisterIndex];
	copyGPar(reinterpret_cast<GPar*>(INTEGER(data)),
		 &(reinterpret_cast<baseSystemState*>(sd->systemSpecific)->dpSaved));	
	restoredpSaved(reinterpret_cast<DevDesc*>(dd));
	copyGPar(&(reinterpret_cast<baseSystemState*>(sd->systemSpecific)->dp),
		 &(reinterpret_cast<baseSystemState*>(sd->systemSpecific)->gp));
	GReset(reinterpret_cast<DevDesc*>(dd));
	break;
    case GE_CheckPlot:
	/* Check that the current plotting state is "valid"
	 */
	sd = dd->gesd[baseRegisterIndex];
	PROTECT(valid = allocVector(LGLSXP, 1));
	/*
	 * If there has not been any base output on the device
	 * then ignore "valid" setting
	 */
	if (reinterpret_cast<baseSystemState*>(sd->systemSpecific)->baseDevice) {
	    LOGICAL(valid)[0] = 
		(reinterpret_cast<baseSystemState*>(sd->systemSpecific)->gp.state == 1) &&
		reinterpret_cast<baseSystemState*>(sd->systemSpecific)->gp.valid;
	} else {
	    LOGICAL(valid)[0] = TRUE;
	}
	UNPROTECT(1);
	result = valid;
	break;
    case GE_ScalePS:
        sd = dd->gesd[baseRegisterIndex];
        dev = dd->dev;
	ddp = &(reinterpret_cast<baseSystemState*>(sd->systemSpecific)->dp);
	ddpSaved = &(reinterpret_cast<baseSystemState*>(sd->systemSpecific)->dpSaved);
	if (isReal(data) && LENGTH(data) == 1) {
	  double rf = REAL(data)[0];
	  ddp->scale *= rf;
	  ddp->cra[0] *= rf; 
	  ddp->cra[1] *= rf;
	  /* Modify the saved settings so effects dislpay list too
	   */
	  ddpSaved->scale *= rf;
	  ddpSaved->cra[0] *= rf; 
	  ddpSaved->cra[1] *= rf;
	}
	else 
	  error(_("Event UpdatePS requires a single numeric value"));
	break;
    }
    return result;
}

/* Register the base graphics system with the graphics engine
 */
void registerBase() {
    GEregisterSystem(baseCallback, &baseRegisterIndex);
}

/* FIXME: Make this a macro to avoid function call overhead?
 */
attribute_hidden
GPar* Rf_gpptr(DevDesc *dd) {
    return &(reinterpret_cast<baseSystemState*>(GEsystemState(reinterpret_cast<GEDevDesc*>(dd), 
							      baseRegisterIndex))->gp);
}

attribute_hidden
GPar* Rf_dpptr(DevDesc *dd) {
    return &(reinterpret_cast<baseSystemState*>(GEsystemState(reinterpret_cast<GEDevDesc*>(dd), 
							      baseRegisterIndex))->dp);
}

attribute_hidden
GPar* Rf_dpSavedptr(DevDesc *dd) {
    return &(reinterpret_cast<baseSystemState*>(GEsystemState(reinterpret_cast<GEDevDesc*>(dd), 
							      baseRegisterIndex))->dpSaved);
}

Rboolean Rf_baseDevice(DevDesc *dd) {
    return reinterpret_cast<baseSystemState*>(GEsystemState(reinterpret_cast<GEDevDesc*>(dd), 
							    baseRegisterIndex))->baseDevice;
}

void Rf_setBaseDevice(Rboolean val, DevDesc *dd) {
    reinterpret_cast<baseSystemState*>(GEsystemState(reinterpret_cast<GEDevDesc*>(dd), 
						     baseRegisterIndex))->baseDevice = val;
}

SEXP Rf_displayList(DevDesc *dd) {
    return (reinterpret_cast<GEDevDesc*>(dd))->dev->displayList;
}
