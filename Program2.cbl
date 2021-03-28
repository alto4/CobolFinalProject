       identification division.
       program-id. FinalProject-DataSplitAndCount.

       author. Scott Alton.
	   date-written. 2021-03-18.

	  ******************************************************************
	  * Program Description: This program generates an error report for
      * all item records being processed. Input from the raw data is 
      * validated to conform to pre-defined business rules, and all 
      * applicable error messages for each item record are provided. 
      * If the record is successfully validated to meet business 
      * requirements, it's successful validity is noted in the report. 
	  ******************************************************************
	   environment division.
	   configuration section.
	   input-output section.

	   file-control.
      * Input file declaration
           select valid-data-file
	           assign to "../../data/VALID-DATA-1-Edit.out"
			   organization is line sequential.

      * Output file declaration
           select sl-data-file
                   assign to "../../data/S&LDataFile.out"
                   organization is line sequential.

		   select r-data-file
				   assign to "../../data/ReturnsDataFile.out"
				   organization is line sequential.

		   select counts-and-controls-file
	   				   assign to "../../data/ReturnsDataFile.out"
	   				   organization is line sequential.
	  ******************************************************************
	   data division.
	   file section.

      * Input record definitions
	   fd valid-data-file
		   record contains 36 characters.
	  
	   01 transaction-record.
		   05 tr-code                    pic x.
		       88 tr-code-valid				 values "S", "R", "L".
		       88 tr-code-s                  value "S".
		       88 tr-code-r                  value "R".
			   88 tr-code-l                  value "L".
		   05 tr-amount                  pic 9(5)v99.
			   88 tr-amount-valid            value 1 thru 99999.99.
		   05 tr-payment-type            pic x(2).
		       88 tr-payment-type-valid 	 values "CA", "CR", "DB".
		       88 tr-payment-type-ca         value "CA".
		       88 tr-payment-type-cr         value "CR".
			   88 tr-payment-type-db         value "DB".
		   05 tr-store-num               pic 99.
		       88 tr-store-num-valid 	     values 01 thru 05, 12.
		   05 tr-invoice-num.
		       10 tr-invoice-num-left-1  pic x.
                   88 tr-invoice-num-left-1-valid
                                             values "A" thru "E".
			   10 tr-invoice-num-left-2  pic x.
                   88 tr-invoice-num-left-2-valid
                                             values "A" thru "E".
	           10 tr-invoice-num-dash    pic x(1).
                   88 tr-invoice-num-dash-valid
                                             value "-".
	           10 tr-invoice-num-right   pic 9(6).
                   88 tr-invoice-num-right-valid
                                             value 100000 thru 900000.
		   05 tr-sku-code				 pic x(15).

	  * Output record definitions
	   fd sl-data-file
		   data record is sl-line
		   record contains 60 characters.

       01 sl-line                        pic x(36).

       fd r-data-file 
           data record is r-line
           record contains 100 characters.

	   01 r-line                         pic x(36).

	   fd counts-and-controls-file
		   data record is report-line
		   record contains 100 characters.
       01 report-line     		pic x(50).
	  ******************************************************************
	   working-storage section.
	   	  
      * Headings 
      * Assignment title heading
       01 ws-heading1-name-line.
		   05 ws-name			         pic x(11) value "Scott Alton".
		   05 filler                     pic x(10) value spaces.
           05 ws-assignment-title	     pic x(13) value "Final Project".
		  
      * Page title heading
	   01 ws-heading2-title.
		   05 filler			         pic x(12) value spaces. 
		   05 ws-title			         pic x(12) value "ERROR REPORT".
		   05 filler                     pic x(10) value spaces.
		   05 ws-page-num                pic 99     value 1.

      * Column headings
	   01 ws-col-headings-1.
		   05 filler                     pic x(1)  value spaces.
		   05 filler                     pic x(6)  value "Record".
		   05 filler                     pic x(2)  value spaces.
           05 filler                     pic x(8)  value "--------".
		   05 filler                     pic x(8)  value "Raw Data".
		   05 filler                     pic x(8)  value "--------".
	  
	   01 ws-col-headings-2.
		   05 filler                     pic x(1)
                                             value spaces.
		   05 filler                     pic x(6)
                                             value "Number".
		   05 filler                     pic x(6)
                                             value spaces.
           05 filler                     pic x(18)
                                             value "and Error Messages".
		   05 filler					 pic x(2)
                                             value spaces.

      * Individual formatted record line
	   01 ws-detail-line. 
           05 filler                     pic x(2)  value spaces.
           05 ws-record-num              pic zz9.
           05 filler                     pic x(4)  value spaces.
		   05 ws-raw-data                pic x(36).
      * Formatted error line
	   01 ws-error-line.
		   05 filler					 pic x(9)  value spaces.
		   05 ws-error-message       	 pic x(25) value spaces.
		   		   	
      * Summary lines
       01 ws-summary.
		   05 ws-summary-hor-rule-line   pic x(34)
                             value "----------------------------------".
		   05 ws-summary-heading		 pic x(19)
							 value " VALIDATION SUMMARY".
           05 ws-input-count-line.
			   10 filler                 pic x
                             value space.
		       10 filler                 pic x(19)
                             value "RECORDS READ COUNT:".
			   10 filler                 pic x(3)
                             value spaces.
               10 ws-input-count         pic 99(3)
                             value 0.
			   10 filler                 pic x(8)
                             value spaces.
		   05 ws-good-count-line.
			   10 filler                 pic x
                             value space.
		       10 filler                 pic x(14)
                             value "VALID RECORDS:".
			   10 filler                 pic x(2)
                             value spaces.
               10 ws-valid-count          pic 9(3)
                             value 0.
			   10 filler                 pic x(15)
                             value spaces.
		   05 ws-error-count-line.
			   10 filler                 pic x
                             value space.
		       10 filler                 pic x(14)
                             value "INVALID COUNT:".
			   10 filler                 pic x
                             value spaces.
               10 ws-error-count         pic 9(3)
                             value 0.
			   10 filler                 pic x(15)
                             value spaces.

	  * Execution display variables
	   01 ws-execution-messages.
		   05 ws-status-message          pic x(31)
                           value "Validating transaction records.".
	       05 ws-output-dest-message     pic x(36)
                           value "Please proceed to the 'data' folder.".
		   05 ws-exit-message            pic x(36)
                           value "Press any key to exit the program...".

      * Counters
       01 ws-counters.
		   05 ws-page-count              pic 99    value 1.
	       05 ws-line-count              pic 99    value 0.
	       05 ws-record-count            pic 9(3)  value 0.
	  * Transaction code counters
		   05 ws-code-s-count			 pic 9(3)  value 0.
		   05 ws-code-r-count			 pic 9(3)  value 0.
           05 ws-code-l-count			 pic 9(3)  value 0.
       
	  * Pagination constants
       77 ws-lines-per-page              pic 9(3)  value 10.

      * Error message content constants
	   77 ws-code-err-msg                pic x(22)
								   value "WRONG TRANSACTION CODE".
	   77 ws-type-error-msg              pic x(18)
	                               value "WRONG PAYMENT TYPE".
	   77 ws-store-num-err-msg           pic x(20)
								   value "INVALID STORE NUMBER".
	   77 ws-inv-num-left-err-msg        pic x(22)
	                               value "INVOICE LEFT INVALID".
	   77 ws-inv-num-left-same-msg       pic x(22)
	   	   						   value "INVOICE LEFT SAME".
	   77 ws-inv-num-right-err-msg       pic x(22)
	   							   value "INVOICE RIGHT INVALID".
	   77 ws-inv-num-no-dash-err-msg     pic x(18)
	   	   						   value "INVOICE NEEDS DASH".
	   77 ws-sku-invalid-err-msg         pic x(11)
	   	   	   					   value "INVALID SKU".
	   77 ws-trans-amount-err-msg        pic x(26)
							       value "INVALID TRANSACTION AMOUNT".
	   77 ws-sku-blank-err-msg           pic x(19)
	   	   	   						   value "SKU CANNOT BE BLANK".
	   77 ws-valid-record-msg            pic x(13)
								   value "*VALID RECORD".
      * Utility constants
       77 ws-one                         pic 9        value 1.
	   77 ws-file-empty                  pic x        value "y".
	   77 ws-eof-flag                    pic x(1)     value "n".
	   77 ws-blank                       pic x        value space.
      ******************************************************************
       procedure division.

       000-main.
           perform 100-open-files.
	   	   perform 200-initial-read.
	   	   perform 300-process-pages
	   		   until ws-eof-flag equals ws-file-empty.
	       perform 600-print-totals.
		   perform 700-close-files.

      * Inform user of program status and exit process
           display ws-status-message.
           
		   display ws-blank.
		   display ws-output-dest-message.
           
		   display ws-blank.
		   display ws-exit-message.
	   
      * End the program
		   accept return-code.

		   goback.

	   100-open-files.    
		   open input  valid-data-file.
		   open output sl-data-file.
		   open output r-data-file.
           open output counts-and-controls-file.

	   200-initial-read.
		   read valid-data-file
		       at end move ws-file-empty
                                      to ws-eof-flag.

       300-process-pages.
	  * Increment the page count and clear output from output lines
		   move ws-page-count         to ws-page-num.
		   move spaces                to report-line.
		   move spaces                to ws-detail-line.
		   move spaces                to ws-error-line.


      * Show report title only on first page, column headers on all
		   if (ws-page-count > ws-one) then
			   write report-line
			       after advancing page
			   perform 310-print-page-header
		   else
			   write report-line
			   perform 330-print-headings
		   end-if.
		         
      * Process data on pages for entire input file
		   perform 400-process-lines
			   varying ws-line-count from ws-one by ws-one
			       until (ws-line-count > ws-lines-per-page
			       or ws-eof-flag = ws-file-empty).

		   add ws-one                 to ws-page-count.

	   310-print-page-header.
		   write report-line from ws-blank.
		   write report-line from ws-heading2-title

		   write report-line from ws-col-headings-1
			 after advancing ws-one line.

		   write report-line from ws-col-headings-2
			 after advancing ws-one line.

		   write report-line from spaces
             after advancing ws-one line.

       320-print-report-header.
		   write report-line from ws-heading1-name-line.

	   330-print-headings.
      * Print both overall report and column headings
		   perform 320-print-report-header.
		   perform 310-print-page-header.

	   400-process-lines.
	  * Assign record number and increment counter
		   add ws-one                 to ws-record-count
             giving ws-record-num.
		   add ws-one                 to ws-record-count.

		   move transaction-record    to ws-raw-data.

		   display ws-detail-line.
      *  Validate all record details according to business rules
		   perform 410-check-trans-code.
		   perform 450-validate-payment-type.
		   perform 451-validate-amount.
		   perform 452-validate-store-num.
		   perform 453-validate-invoice-num-left. 
		   perform 454-validate-invoice-num-right.
		   perform 455-validate-invoice-num-center. 
		   perform 456-validate-sku-code.

	  * Declare record's valid status if no errors arise
		   if (ws-error-message = ws-blank) then
	  *		   write to data file here
			   write r-line from ws-raw-data
			   add ws-one to ws-valid-count
			   subtract ws-one from ws-line-count
		   else
	  *			Writing report line
			   write sl-line from ws-raw-data
			   add ws-one to ws-error-count
		   end-if.


		   move spaces to ws-error-message.


		   read valid-data-file
		       at end move ws-file-empty
                                      to ws-eof-flag.
		   
	   410-check-trans-code. 
      * Increment valid item code counters, or produce code error
		   if (tr-code-s) then
			   add ws-one             to ws-code-s-count
		   else if (tr-code-r) then
			   add ws-one             to ws-code-r-count
		   else if (tr-code-l) then
			   add ws-one             to ws-code-l-count
		   else 
			   perform 460-check-invalid-record-output
			   move ws-code-err-msg   to ws-error-message
			   perform 461-display-error
           end-if.

	   450-validate-payment-type.
		   if (not tr-payment-type-valid) then
			   perform 460-check-invalid-record-output
			   move ws-type-error-msg
                                      to ws-error-message
			   perform 461-display-error
		   end-if.

	   451-validate-amount.
		   if (not tr-amount-valid or not tr-amount is numeric)
			   perform 460-check-invalid-record-output
			   move ws-trans-amount-err-msg to ws-error-message
			   perform 461-display-error
		   end-if.

	   452-validate-store-num.
		   if (not tr-store-num-valid)
			   perform 460-check-invalid-record-output
			   move ws-store-num-err-msg to ws-error-message
			   perform 461-display-error
		   end-if.

	   453-validate-invoice-num-left. 
		   if (tr-invoice-num-left-1 equals tr-invoice-num-left-2)
			   perform 460-check-invalid-record-output
			   move ws-inv-num-left-same-msg to ws-error-message
			   perform 461-display-error
		   end-if.

		   if (not tr-invoice-num-left-1-valid and
			   not tr-invoice-num-left-2-valid) or 
              (not tr-invoice-num-left-1 alphabetic-upper) or 
		      (not tr-invoice-num-left-2 alphabetic-upper)
			   perform 460-check-invalid-record-output
			   move ws-inv-num-left-err-msg to ws-error-message
			   perform 461-display-error
		   end-if.
		   
	   454-validate-invoice-num-right.
		    if (not tr-invoice-num-right-valid) or
			  (tr-invoice-num-right not numeric)
			   perform 460-check-invalid-record-output
			   move ws-inv-num-right-err-msg to ws-error-message
			   perform 461-display-error
		   end-if.

	   455-validate-invoice-num-center. 
		   if (not tr-invoice-num-dash-valid)
		       perform 460-check-invalid-record-output
			   move ws-inv-num-no-dash-err-msg to ws-error-message
			   perform 461-display-error
		   end-if.

	   456-validate-sku-code. 
		   if (tr-sku-code = ws-blank)
			   perform 460-check-invalid-record-output
			   move ws-sku-blank-err-msg to ws-error-message
			   perform 461-display-error
		   end-if.

	   460-check-invalid-record-output.
		   if (ws-error-line = ws-blank)
			   write report-line from ws-detail-line
				 after advancing ws-one line
		   end-if.
	   461-display-error.
		   write report-line from ws-error-line.


	   600-print-totals.
	  * Print total values
		   move ws-record-count       to ws-input-count.

      * Format summary section of report
		   write report-line from ws-summary-hor-rule-line
			 after advancing ws-one lines.
		   write report-line from ws-summary-heading
		     after advancing ws-one lines.
		   write report-line from ws-blank
			 after advancing ws-one lines.

      * Display required totals in summary
		   write report-line from ws-input-count-line
	   		 after advancing ws-one lines.
		   write report-line from ws-good-count-line
			 after advancing ws-one lines.
		   write report-line from ws-error-count-line
			 after advancing ws-one lines.
	
	   700-close-files.
		   close valid-data-file, sl-data-file, r-data-file,
             counts-and-controls-file.

	  ******************************************************************
	   end program FinalProject-DataSplitAndCount.
