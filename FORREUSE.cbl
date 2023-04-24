      ******************************************************************
      *    THIS PART OF CODE IS NOT NEEDED
      *    WE TAKE CURRENT DATE STRAIGHT FROM DB2 NOT BY ZOS SERVICE
      *     IF WS-DB2-DATE-FORMAT(5:1) = '-' THEN
      *        *> YYYY/MM/DD
      *        DISPLAY 'TEST'
      *     ELSE 
      *        IF WS-DB2-DATE-FORMAT(3:1) = '/' THEN
      *           *> MM/DD/YYYY
      *           DISPLAY 'TESY'
      *        ELSE
      *           *> MM/DD/YYY
      *           DISPLAY 'TEST'
      *        END-IF
      *     END-IF
      *    END CHECKING DB2 DATE FORMAT  
      ******************************************************************