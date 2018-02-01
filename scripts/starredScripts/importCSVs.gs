/* Import csv to spreadsheet keeping previous format
*/
function importCSV() {
  
   // Import .csv file from URL to the current active sheet keeping format
  // Provide the full URL of the CSV file available at chat format -> advanced -> Expose public csv URL
  //
  // Clear current data keeping format
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var sheet1 = ss.getSheetByName('Sheet1');
  SpreadsheetApp.getActiveSheet().getRange(1,1,sheet1.getMaxRows(),sheet1.getMaxColumns()).clear({contentsOnly: true});
  //
  // update clear spreadsheet
  SpreadsheetApp.flush();
  //
  // import csv
  var csvUrl = "https://app.periscopedata.com/api/iguanafix/chart/csv/c8a78d9b-ed7c-11bb-ac7d-562f9ddb7fa4/322144";
  var csvContent = UrlFetchApp.fetch(csvUrl).getContentText();
  var csvData = Utilities.parseCsv(csvContent);  
  var sheet = SpreadsheetApp.getActiveSheet();
  sheet.getRange(1, 1, csvData.length, csvData[0].length).setValues(csvData);

}
