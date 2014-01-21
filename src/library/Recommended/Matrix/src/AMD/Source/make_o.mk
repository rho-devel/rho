OBJS = amd_i_aat.o amd_l_aat.o amd_i_1.o amd_l_1.o amd_i_2.o amd_l_2.o amd_i_postorder.o amd_l_postorder.o amd_i_post_tree.o amd_l_post_tree.o amd_i_defaults.o amd_l_defaults.o amd_i_order.o amd_l_order.o amd_i_control.o amd_l_control.o amd_i_info.o amd_l_info.o amd_i_valid.o amd_l_valid.o amd_i_preprocess.o amd_l_preprocess.o amd_i_dump.o amd_l_dump.o amd_i_global.o amd_l_global.o 

amd_i_aat.o: amd_aat.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDINT -c amd_aat.c -o $@
amd_i_1.o: amd_1.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDINT -c amd_1.c -o $@
amd_i_2.o: amd_2.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDINT -c amd_2.c -o $@
amd_i_postorder.o: amd_postorder.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDINT -c amd_postorder.c -o $@
amd_i_post_tree.o: amd_post_tree.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDINT -c amd_post_tree.c -o $@
amd_i_defaults.o: amd_defaults.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDINT -c amd_defaults.c -o $@
amd_i_order.o: amd_order.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDINT -c amd_order.c -o $@
amd_i_control.o: amd_control.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDINT -c amd_control.c -o $@
amd_i_info.o: amd_info.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDINT -c amd_info.c -o $@
amd_i_valid.o: amd_valid.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDINT -c amd_valid.c -o $@
amd_i_preprocess.o: amd_preprocess.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDINT -c amd_preprocess.c -o $@
amd_i_dump.o: amd_dump.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDINT -c amd_dump.c -o $@
amd_i_global.o: amd_global.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDINT -c amd_global.c -o $@

amd_l_aat.o: amd_aat.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c amd_aat.c -o $@
amd_l_1.o: amd_1.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c amd_1.c -o $@
amd_l_2.o: amd_2.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c amd_2.c -o $@
amd_l_postorder.o: amd_postorder.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c amd_postorder.c -o $@
amd_l_post_tree.o: amd_post_tree.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c amd_post_tree.c -o $@
amd_l_defaults.o: amd_defaults.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c amd_defaults.c -o $@
amd_l_order.o: amd_order.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c amd_order.c -o $@
amd_l_control.o: amd_control.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c amd_control.c -o $@
amd_l_info.o: amd_info.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c amd_info.c -o $@
amd_l_valid.o: amd_valid.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c amd_valid.c -o $@
amd_l_preprocess.o: amd_preprocess.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c amd_preprocess.c -o $@
amd_l_dump.o: amd_dump.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c amd_dump.c -o $@
amd_l_global.o: amd_global.c $(INC)
	$(CC) $(ALL_CPPFLAGS) $(ALL_CFLAGS) -I../Include -DDLONG -c amd_global.c -o $@
