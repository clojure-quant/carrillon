(ns carillon.pdf
  (:import
    [java.io PushbackReader InputStream InputStreamReader OutputStream
             FileOutputStream ByteArrayOutputStream File]
    [javax.imageio ImageIO]
    [com.lowagie.text Chunk Document HeaderFooter Phrase Rectangle RectangleReadOnly
                      PageSize Font FontFactory Paragraph Image ]
    [com.lowagie.text.pdf BaseFont PdfContentByte PdfReader
                          PdfStamper PdfReader PdfWriter PdfCopy
                          PdfName ]
    [com.lowagie.text.pdf.parser PdfTextExtractor]
   ))


 new PdfTextExtractor (pdfReader) .getTextFromPage (pageNumber);

(let [inputfile "2022.pdf"
      reader (PdfReader. inputfile)
      pc (.getNumberOfPages reader)
      page (.getPageN reader 1) 
      resources (.getAsDict page (PdfName. "Resources"))
      extractor (PdfTextExtractor. reader) 
      ;content (.getResource page (PdfName. "Content"))
      ]
  (println "pages: " pc)
  (.getKeys page)
  (.getTextFromPage extractor 1 false)
  resources
  
   
  )


int i = 0;            
+ "-" + String.format ("%03d", i + 1) + ".pdf"; 
System.out.println ("Writing " + outFile);
Document document = new Document (reader.getPageSizeWithRotation (1));

(require '[pdfboxing.text :as text])

(println (text/extract "2022.pdf"))

DEPOSITO COMPLETO ()08/11/2022 265649501 30 24,191.33105.00
N/C: ACH(LUIS EDUARDO ANG Pago nov 2022\)09/11/2022 1329041835 0 24,296.33105.00
DEPOSITO COMPLETO ()12/11/2022 284948830 25 24,396.33100.00 
              
              
              ([\w\s]+)\((.*)\)([\w\/]*)\s(\d*)\s(\d*)\s([\d,]*.\d\d)([\d,]*.\d\d)
              
              
              