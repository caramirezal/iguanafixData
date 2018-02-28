/* send attached pdf with not gridlines  
*/
function importCSV() {
  
  // get report date
  var utc = new Date().toJSON().slice(0,10).replace(/-/g,'/');
  
  
   // Import .csv file from URL to the current active sheet keeping format
  // Provide the full URL of the CSV file available at chat format -> advanced -> Expose public csv URL
  //
  // Clear current data keeping format
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var sheet1 = ss.getSheetByName('Sheet1');
  SpreadsheetApp.getActiveSheet().getRange(5,1,sheet1.getMaxRows(),sheet1.getMaxColumns()).clear({contentsOnly: true});
  //
  // update clear spreadsheet
  SpreadsheetApp.flush();
  //
  // import csv
  var csvUrl = "https://app.periscopedata.com/api/iguanafix/chart/csv/6fd6a63e-ca6d-a687-bf5d-478bc5c8ca8b";
  var csvContent = UrlFetchApp.fetch(csvUrl).getContentText();
  var csvData = Utilities.parseCsv(csvContent);  
  var sheet = SpreadsheetApp.getActiveSheet();
  sheet.getRange(1+4, 1, csvData.length, csvData[0].length).setValues(csvData);
  sheet.getRange('F1').setValue(utc);
  
    // update spreadsheet
  SpreadsheetApp.flush();
 
  
    // Send email with the current active spreadsheet attached 
  try {
    var ss = SpreadsheetApp.getActive();    
    var url = "https://docs.google.com/feeds/download/spreadsheets/Export?key=" + ss.getId() + "&gridlines=false&portrait=false&exportFormat=pdf";
    var params = {
      method      : "get",
      headers     : {"Authorization": "Bearer " + ScriptApp.getOAuthToken()},
      muteHttpExceptions: true
    };
    
    var blob = UrlFetchApp.fetch(url, params).getBlob();
    
    blob.setName(ss.getName() + ".pdf");
    
    // Edit email recipients, subject and body message 
    MailApp.sendEmail("carlos.ramirez@iguanafix.com,jose.ceron@iguanafix.com",   // Recipients 
                      "Reporte Semanal IguanaFix test",                                 // Subject
                      "Estimado usuario: \n \n     Su reporte está listo!! Gracias por su confianza en IguanaFix.",      // Body message
                      {attachments: [blob]});
    
  } catch (f) {
    Logger.log(f.toString());
  }

}

