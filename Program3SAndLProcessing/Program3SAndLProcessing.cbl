              identification division.
       program-id. Program3SAndLProcessing.

       input-output section.
       file-control.
      * TODO: correct file paths if needed
                  select input-file
                      assign to "../../../data/S&LDataFile.out"
                      organization is line sequential.

                  select output-file
                      assign to "../../../data/ReturnRecord.out"
                      organization is line sequential.
       configuration section.

       data division.
       file section.
                     fd input-file
                         data record is sales-line
                         record contains 32 characters.

       01 input-rec.
         05 tr-code pic x.
           88 tr-code-valid values "S", "R", "L".
           88 tr-code-s value "S".
           88 tr-code-r value "R".
           88 tr-code-l value "L".

         05 tr-amount pic 9(5)v99.
           88 tr-amount-valid value 1 thru 99999.99.

         05 tr-payment-type pic x(2).
           88 tr-payment-type-valid values "CA", "CR", "DB".
           88 tr-payment-type-ca value "CA".
           88 tr-payment-type-cr value "CR".
           88 tr-payment-type-db value "DB".

         05 tr-store-num pic 99.
           88 tr-store-num-valid values 01 thru 05, 12.

         05 tr-invoice-num.
           10 tr-invoice-num-left-1 pic x.
             88 tr-invoice-num-left-1-valid
                                                 values "A" thru "E".
           10 tr-invoice-num-left-2 pic x.
             88 tr-invoice-num-left-2-valid
                                                values "A" thru "E".
           10 tr-invoice-num-dash pic x(1).
             88 tr-invoice-num-dash-valid
                                                  value "-".
           10 tr-invoice-num-right pic 9(6).
             88 tr-invoice-num-right-valid
                                              value 100000 thru 900000.
         05 tr-sku-code pic x(15).

       fd output-file
            data record is report-line
            record contains 120 characters.

       01 output-line pic x(350).

       working-storage section.

       01 ws-eof-flag pic x value "N".
      *
       01 ws-heading1-name-line.
         05 ws-name pic x(11) value "Scott Alton".
         05 filler pic x(10) value spaces.
         05 ws-assignment-title pic x(13) value "Final Project".
      *
       01 ws-page-heading1.

         05 filler pic x(8) value spaces.
         05 filler pic x(24) value "ERROR REPORT".
         05 filler pic x(6) value spaces.
         05 filler pic x(5) value "Page:".
         05 ws-page-number pic z9.

       01 ws-page-heading2.
         05 FILLER PIC x(10) VALUE "CODE".
         05 filler pic x(3) VALUE SPACES.
         05 FILLER PIC x(10) VALUE "AMOUNT".
         05 filler pic x(2) VALUE SPACES.
         05 FILLER PIC x(4) VALUE "TYPE".
         05 filler pic x(2) VALUE SPACES.
         05 FILLER PIC x(10) VALUE "STORE NUM".
         05 filler pic x(2) VALUE SPACES.
         05 FILLER PIC x(11) VALUE "INVOICE NUM".
         05 filler pic x(5) VALUE SPACES.
         05 FILLER PIC x(7) VALUE "SKUCODE".
         05 filler pic x(9) VALUE SPACES.
         05 FILLER PIC x(10) VALUE "TAX OWING".

       01 ws-detail-line.
         05 ws-code pic x(1).
         05 filler pic x(12) VALUE SPACES.
         05 ws-amount pic 9(5)v99.
         05 filler pic x(5) VALUE SPACES.
         05 ws-payment-type pic x(2).
         05 filler pic x(8) VALUE SPACES.
         05 ws-store-num pic 99.
         05 filler pic x(12) VALUE SPACES.
         05 ws-invoice-num pic x(1).
         05 filler pic x(8) VALUE SPACES.
         05 ws-sku-code pic x(15).
         05 filler pic x(3) VALUE SPACES.
         05 ws-tax-owing pic 9(5)v99.
         05 filler pic x(3) VALUE SPACES.

       01 ws-sl-record.
         05 filler pic x(50) value
                   "Total number of S&L records and Total Amount".
         05 ws-sl-total pic 9(8)v99.

       01 ws-s-record.
         05 filler pic x(50) value
                   "Total number of S records and Total Amount".
         05 ws-s-total pic 9(8)v99.
       01 ws-l-record.
         05 filler pic x(50) value
                   "Total number of L records and Total Amount".
         05 ws-l-total pic 9(8)v99.

       01 ws-calc.
         05 ws-temp-tax-owing pic 9(5)v99.
         05 ws-temp-S-total-amount pic 9(7)v99 value 0.
         05 ws-temp-L-total-amount pic 9(7)v99 value 0.
         05 ws-temp-CA pic 9(3).
         05 ws-temp-CR pic 9(3).
         05 ws-temp-DB pic 9(3).
         05 ws-temp-type pic 9(3).
         05 ws-total-tax-owing pic 9(6)v99.
         05 ws-temp-count pic 9(2)v99.
         05 ws-temp-percentage pic 9(3)v99.

      *
       77 ws-lines-per-page pic 99 value 20.
       77 ws-page-count pic 99 value 0.
       77 ws-line-count pic 99 value 0.
       77 ws-file-empty pic x value "e".
       77 ws-file-opened pic x value "o".
       77 ws-zero pic 9 value 0.
       77 ws-one pic 9 value 1.
       77 ws-two pic 9 value 2.
      *

       procedure division.

      * Main Method
       000-Main.

           perform 100-open-files.
           perform 200-write-report-headings.
           perform 300-read-file.
           perform 400-process-pages
             until ws-eof-flag equals ws-file-empty.
           perform 500-write-report-footers.
           perform 600-close-files.
           goback.

      * open files to read and write
       100-open-files.
           open input input-file.
           open output output-file.
           move ws-file-opened to ws-eof-flag.
      * write report - heading
       200-write-report-headings.
           write output-line from ws-heading1-name-line
             after advancing ws-one line.
      * read file
       300-read-file.
           read input-file
               at end
                   move ws-file-empty to ws-eof-flag.
      * paging 10 lines per page
       400-process-pages.
           perform 410-write-page-headings.

           perform 450-process-lines
             varying ws-line-count from ws-one by ws-one
             until (ws-line-count > ws-lines-per-page
             OR ws-eof-flag = ws-file-empty).

       410-write-page-headings.
           add ws-one to ws-page-count.
           move ws-page-count to ws-page-number.
           if (ws-page-count > ws-one) then
               write output-line from ws-page-heading1
                 after advancing page
               write output-line from spaces
               write output-line from ws-page-heading2
               write output-line from spaces
           else
               write output-line from ws-page-heading1
               write output-line from spaces
               write output-line from ws-page-heading2
               write output-line from spaces
           end-if.

       450-process-lines.
           perform 457-write-detail-line.
           perform 300-read-file.

       457-write-detail-line.
           perform 460-calculate-tax-owing.

      *
      *    move detail output data
           move tr-code to ws-code.
           move tr-amount to ws-amount.
           move tr-payment-type to ws-payment-type.
           move tr-store-num to ws-store-num.
           move tr-invoice-num to ws-invoice-num.
           move tr-sku-code to ws-sku-code.
           move ws-temp-tax-owing to ws-tax-owing.

      *
      * write detail output
           write output-line from ws-detail-line after advancing 2 line.

      * Calculate Tax owing
       460-calculate-tax-owing.

           multiply tr-amount by 0.13 giving ws-temp-tax-owing.

      * footer summary calculation
       500-write-report-footers.
           perform 510-calculate-record-total.
           perform 520-calculate-payment-type.

      * calculate percentage of type

           divide ws-temp-CA by ws-temp-type giving ws-temp-count.
           multiply ws-temp-count by 100 giving ws-temp-percentage.

           move ws-temp-S-total-amount to ws-s-total.
           move ws-temp-L-total-amount to ws-l-total.
           add ws-s-total to ws-l-total giving ws-sl-total.

           write output-line from ws-s-record after advancing 1 line.
           write output-line from ws-l-record after advancing 2
             line.
           write output-line from ws-sl-record after advancing 2 line.

       510-calculate-record-total.
           if (tr-code-s) then
               add tr-amount to ws-temp-S-total-amount
           else
               if (tr-code-l) then
                   add tr-amount to ws-temp-L-total-amount.

       520-calculate-payment-type.

           add ws-one to ws-temp-type.

           if (tr-payment-type-ca) then

           end-if
           add ws-one to ws-temp-CA
           if (tr-payment-type-cr) then
               add ws-one to ws-temp-CR
           else
               if (tr-payment-type-db) then
                   add ws-one to ws-temp-DB.

      * close file
       600-close-files.
           accept return-code.
           close input-file, output-file.

       end program Program3SAndLProcessing.