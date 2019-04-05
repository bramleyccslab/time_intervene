//Generic helper functions!


//Create a upi
function makeid()
{
    var text = "";
    var possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

    for( var i=0; i < 5; i++ )
        text += possible.charAt(Math.floor(Math.random() * possible.length));

    return text;
}

//Shuffle an array
function shuffle(array) {
	var currentIndex = array.length, temporaryValue, randomIndex;

  // While there remain elements to shuffle...
  while (0 !== currentIndex) {

    // Pick a remaining element...
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex -= 1;

    // And swap it with the current element.
    temporaryValue = array[currentIndex];
    array[currentIndex] = array[randomIndex];
    array[randomIndex] = temporaryValue;
}

return array;
}


//Move forward through the experiment's main "slides"/phases
function ClickContinue(hide, show)
{
	document.getElementById(hide).style.display="none";
	document.getElementById(show).style.display = "block";
	window.scrollTo(0,0);        
}



function exportToCsv(myCsv) {
            window.open('data:text/csv;charset=utf-8,' + escape(myCsv));
            console.log('hi');
        }



