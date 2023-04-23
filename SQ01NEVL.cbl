           EVALUATE WS-QREC01-SQ-ROWSET-MD 
              WHEN 0413 
                 SET INX-N TO 27
                 MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1 
              WHEN 0414 THRU 0426 
                 SET INX-N TO 1
                 MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0427
                 SET INX-N TO 1
                 MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0428 THRU 0510
                 SET INX-N TO 2
                 MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0511
                 SET INX-N TO 2
                 MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
      *
              WHEN 0512 THRU 0524
                 SET INX-N TO 3
                 MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0525
                 SET INX-N TO 3
                 MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0526 THRU 0607
              SET INX-N TO 4
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0608
              SET INX-N TO 4
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0609 THRU 0620
              SET INX-N TO 5
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0621
              SET INX-N TO 5
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0622 THRU 0704
              SET INX-N TO 6
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0705
              SET INX-N TO 6
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0706 THRU 0718
              SET INX-N TO 7 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0719
              SET INX-N TO 7
              MOVE 1 TO WS-NU  
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1              
              WHEN 0720 THRU 0801
              SET INX-N TO 8 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0802 
              SET INX-N TO 8 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0803 THRU 0815
              SET INX-N TO 9 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0816
              SET INX-N TO 9
              MOVE 1 TO WS-NU  
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0817 THRU 0829
              SET INX-N TO 10 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0830
              SET INX-N TO 10
              MOVE 1 TO WS-NU  
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0831 THRU 0912
              SET INX-N TO 11 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0913
              SET INX-N TO 11 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0914 THRU 0925
              SET INX-N TO 12 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0926
              SET INX-N TO 12 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1              
              WHEN 0927 THRU 1009
              SET INX-N TO 13 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 1010
              SET INX-N TO 13 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 1011 THRU 1022
              SET INX-N TO 14 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 1023
              SET INX-N TO 14 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 1024 THRU 1105 
              SET INX-N TO 15 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 1106 
              SET INX-N TO 15 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 1107 THRU 1118 
              SET INX-N TO 16 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 1119 
              SET INX-N TO 16 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 1120 THRU 1201 
              SET INX-N TO 17 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 1202 
              SET INX-N TO 17 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1              
              WHEN 1203 THRU 1214 
              SET INX-N TO 18 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 1215 
              SET INX-N TO 18 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 1216 THRU 1227 *> MULA
              SET INX-N TO 19 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 1228 
              SET INX-N TO 19 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
      *        WHEN 1229 THRU 0110 *> REVERTES DATE DECEMBER -> JANUARY
              WHEN 1229 THRU 1231  *> DEC-JUN
                 SET INX-N TO 20 
                 MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0101 THRU 0110  *> DEV-JUN
              SET INX-N TO 20 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0111 
              SET INX-N TO 20 
              MOVE 1 TO WS-NU  
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0112 THRU 0123  *> UTTARA ASZADHA
                 SET INX-N TO 21 
                 MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0124
              SET INX-N TO 21 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0125 THRU 0205
              SET INX-N TO 22 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0206
              SET INX-N TO 22 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1              
              WHEN 0207 THRU 0218
              SET INX-N TO 23 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0219
              SET INX-N TO 23 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0220 THRU 0303 
              SET INX-N TO 24 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0304 
              SET INX-N TO 24 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0305 THRU 0316 
              SET INX-N TO 25 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0317 
              SET INX-N TO 25 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0318 THRU 0330
              SET INX-N TO 26 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
              WHEN 0331 
              SET INX-N TO 26 
              MOVE 1 TO WS-NU 
                 COMPUTE WS-U(I,INX-N) = WS-U(I,INX-N) + 1
              WHEN 0401 THRU 0412 
              SET INX-N TO 27 
              MOVE 0 TO WS-NU 
                 COMPUTE WS-N(I,INX-N) = WS-N(I,INX-N) + 1
           END-EVALUATE