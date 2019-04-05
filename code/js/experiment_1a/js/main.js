
//////////////////
//Set up variables
//////////////////
var learn_cond = ['active','passive'][0];//[Math.floor(Math.random()*2)];
var delay_cond = ['rel_within_var_between', 'var_both', 'rel_both'][2];//[Math.floor(Math.random()*3)];//, 'var_both', 'rel_both'

console.log('learn_cond',learn_cond,'delay_cond',delay_cond);

var networks = [
// [[0,0,1],[0,0,1],[0,0,0]],
// [[0,1,1],[0,0,0],[0,0,0]],
// [[0,1,0],[0,0,1],[0,0,0]],

//  [[0,0,0,1],[0,0,0,1],[0,0,0,1],[0,0,0,0]],
//  [[0,1,1,1],[0,0,0,0],[0,0,0,0],[0,0,0,0]],
//  [[0,1,0,0],[0,0,1,0],[0,0,0,1],[0,0,0,0]],

//  [[0,1,0],[0,0,1],[1,0,0]],
//  [[0,1,1],[1,0,0],[0,0,0]],
//  [[0,1,0],[0,0,1],[0,1,0]],

//  [[0,1,0,0],[0,0,1,0],[0,0,0,1],[1,0,0,0]],
//  [[0,1,0,0],[1,0,0,0],[0,0,0,1],[0,0,1,0]],
//  [[0,1,0,0],[0,0,1,0],[0,1,0,1],[0,0,0,0]]

[[0,0,1],[0,0,1],[0,0,0]],
[[0,1,0],[0,0,1],[0,0,0]],
[[0,1,1],[0,0,0],[0,0,0]],

[[0,0,0,1],[0,0,0,1],[0,0,0,1],[0,0,0,0]],
[[0,1,0,0],[0,0,1,0],[0,0,0,1],[0,0,0,0]],
[[0,1,1,1],[0,0,0,0],[0,0,0,0],[0,0,0,0]],

[[0,1,0], [0,0,1], [0,1,0]],
[[0,1,0], [0,0,1], [1,0,0]],
[[0,1,0], [1,0,1], [0,0,0]],

[[0,1,0,0], [0,0,1,0], [0,1,0,1], [0,0,0,0]],
[[0,1,0,0], [0,0,1,0], [0,0,0,1], [1,0,0,0]],
[[0,1,0,0], [0,0,1,0], [1,0,0,1], [0,0,0,0]]
];//By row


var trial_counter = 0;

var N = undefined;
var max_N = 4; //For saving the data how big is the biggest model in the exp?
var locMode  = 'even';
var timeout = 45;
var background = 0;//0.05;

if (learn_cond=='passive')
{
	var which_subject_background = 'fixed';
} else {
	var which_subject_background = 'fixed';//'roots';
}

var contingency = 0.9;

var mode = 'test';
var trial_order = shuffle([0,1,2,3,4,5,6,7,8,9,10,11]);//,10,11]);//,12,13,14]);

trial_order.unshift(0);//Add a practice trial

var n_trials = trial_order.length;//10
console.log('trial_order', trial_order);

var node_positions = new Array();

var n_ints = 6; //How many fixed random interventions do passive particpiants see and can active participants perform?

var tmp = [0,1,2,3];
for (var i = 0; i < trial_order.length; i++) {
	node_positions[i] = shuffle(tmp.slice(0, networks[trial_order[i]].length));
}
console.log('node positions',node_positions);

var event_data = [];//All the events in each trial and their time stamps and origins
var belief_data = [];//
var trial_data = []; //The trial number, the problem number, the final judgment, its accuracy
//Incorporate condition dependency for the below

var delay_mu = 1500;

if (delay_cond=='rel_both')
{
	//Reliable
	var delay_alpha = 200;
	var example_network = [[0,1,0],[1,0,1],[0,0,0]];

	document.getElementById('rel_both_ins2').style = "display:block";

	document.getElementById('example_pic').setAttribute('src', 'images/example_emitter.png');
	
	document.getElementById('condition_dependent_comprehension_question').innerHTML = '\"Connections sometimes work a bit faster or slower than other times.\"';

} else if (delay_cond=='var_both'){
	//Within
	document.getElementById('var_both_ins2').style = "display:block";
	var delay_alpha = 5;
	var example_network = [[0,1,0],[1,0,1],[0,0,0]];
	document.getElementById('example_pic').setAttribute('src', 'images/example_emitter.png');

	document.getElementById('condition_dependent_comprehension_question').innerHTML = '\"Connections sometimes work much faster or slower than other times.\"';

} else {
	//Between
	document.getElementById('rel_within_var_between_ins2').style = "display:block";
	var delay_alpha = 5;
	var example_network = [[0,1,0],[1,0,1],[0,0,0]];
	document.getElementById('example_pic').setAttribute('src', 'images/example_emitter.png');

	document.getElementById('condition_dependent_comprehension_question').innerHTML = '\"Some connections work faster than others.\"';
}



var pay_rate = 5;
var upi = makeid();
document.getElementById('display-upi').innerHTML = "Code for mechanical turk:  " + upi;
var total_score = 0;
var total_bonus = 0;

///////////
//Functions
///////////


//Button handling



//Conditionally move past the comprehension check or back to the beginning

function ContinueComprehension()
{
	var q1 = $('input[name=q1]:checked').val();
	var q2 = $('input[name=q2]:checked').val();
	var q3 = $('input[name=q3]:checked').val();
	var q4 = $('input[name=q4]:checked').val();
	var q5 = $('input[name=q4]:checked').val();

	if (typeof q1 != 'undefined' & typeof q2 != 'undefined' &
		typeof q3 != 'undefined' & typeof q4 != 'undefined' &
		typeof q5 != 'undefined')
	{
		document.getElementById("comprehension").disabled = false;
	}
}

function CheckComprehension()
{


	var comprehension_passed = 0;

	if (document.getElementById("q1cor").checked==true & 
		document.getElementById("q2cor").checked==true & 
		document.getElementById("q3cor").checked==true & 
		document.getElementById("q4cor").checked==true & 
		document.getElementById("q5cor").checked==true)
	{
		comprehension_passed=1;

	} else {
		comprehension_passed =0;
	}

	if (comprehension_passed==1)
	{
		document.getElementById("comprehension_check").style.display="none";
		document.getElementById("comprehension_pass").style.display = "block";
		window.scrollTo(0,0);
		
		console.log('passed comprehension');

		NextTrial();
	}   else
	{
		document.getElementById("comprehension_check").style.display="none";
		document.getElementById("comprehension_fail").style.display = "block";

		var tmp = '<br>';
		var questions = ["q1q","q2q","q3q","condition_dependent_comprehension_question","q5q"];
		var answers = ["q1cor","q2cor","q3cor","q4cor","q5cor"];
		var qnames = ["q1","q2","q3","q4","q5"];
		for (var i=0; i<5; ++i)
		{
			if (document.getElementById(answers[i]).checked==false)
			{
				tmp+= "(" + (i+1) + ")" + document.getElementById(questions[i]).innerHTML + '?<br><br>';
				
				tmp2 = 'input[name=' + qnames[i] + ']';
				$(tmp2).prop('checked', false);
			}
			console.log(tmp);
		}


		document.getElementById("comprehension_failed_questions").innerHTML = tmp;

        // $('input[name=q1]').prop('checked', false);
        // $('input[name=q2]').prop('checked', false);
        // $('input[name=q3]').prop('checked', false);
        // $('input[name=q4]').prop('checked', false);
        // $('input[name=q5]').prop('checked', false);

        window.scrollTo(0,0);

    }  
}

//Set the params for the next trial ready to go
function NextTrial()
{

	//Prompts
	if (delay_cond =='rel_within_var_between')
	{
		var tmpTxt = 'between the connections.';
	} else {
		var tmpTxt = 'on different occasions.';
	}
	
	if(trial_counter==0)
	{
		var tmpTxt2 = "(PRACTICE) DEVICE " + (trial_counter+1) + 
		"<br>\u2022 The connections work 90% of the time but the delays vary " +  tmpTxt +
		"<br>\u2022 You can activate components up to 6 times (left click on them)." +
		"<br>\u2022 Mark where you think the connections go as quickly as possible (left clicking in the gray boxes and confirming your changes)." +
		"<br>\u2022 You can see your time remaining at the top left and your tests remaining in the bottom right.<br>";
	} else {
		var tmpTxt2 = "DEVICE " + (trial_counter+1) +
		"<br>\u2022 The connections work 90% of the time but the delays vary " +  tmpTxt +
		"<br>\u2022 You can activate components up to 6 times (left click on them)." +
		"<br>\u2022 Mark where you think the connections go as quickly as possible (left clicking in the gray boxes and confirming your changes)." +
		"<br>\u2022 You can see your time remaining at the top left and your tests remaining in the bottom right.<br>";
	}
	
	$("#prompt-text").html(tmpTxt2);

	var html = 'Click <span style="font-weight: bold">Start</span> to begin';
	$('#lower-prompt-text').html(html) ;

	if (trial_counter<n_trials)
	{
		console.log('Next trial, trial_counter:', trial_counter,
			'trial_id:', trial_order[trial_counter]);

		//Update progress bar
		var width = 2 + 98 * ((trial_counter) / (n_trials - 1.0));
		$("#indicator-stage").css({"width": width + "%"});
		$("#progress-text").html("Progress " + (trial_counter + 1) + "/" + n_trials);

		iframe = document.getElementById("game-frame");
		console.log('iframe',iframe, iframe.src);

		iframe.src = iframe.src; //I think this refreshes the iframe

		prob_N = trial_order[trial_counter];
		network = networks[trial_order[trial_counter]];
		node_position = node_positions[trial_counter];
		N=network.length;

		params = {
			
			N: N,
			max_N: max_N,
			prob_N: prob_N,
			network: network,
			node_position: node_position,
			mode: mode,
			learn_cond: learn_cond,
			delay_cond: delay_cond,
			locMode: locMode,
			timeout: timeout,
			background: background,
			contingency: contingency,
			which_subject_background: which_subject_background,
			delay_mu: delay_mu,
			delay_alpha: delay_alpha,
			n_ints: n_ints
		};

		document.getElementById("start_button").disabled = false;
		//document.getElementById("submit_button").disabled = true;
		document.getElementById("trial_next_button").disabled = true;
	} else {

		var html = "<p>Congratulations!</p><p>Based on a randomly chosen time points during each problem, you marked and confirmed <span style='font-weight:bold'>" + String(total_bonus) + "</span> causal connections correctly.</p>" + 
		"<p>This means that your bonus will be <span style='font-weight:bold'>" + String(total_bonus*pay_rate) +
		" cents</span>.</p>";

		console.log('html', html);

		$('.feedback_text').html(html) ;

		ClickContinue('trial', 'performance_feedback');
	}
	
}

function StartTrial()
{
	trial_counter = trial_counter + 1;
	
	var html = 'Trial ongoing...';
	$('#lower-prompt-text').html(html);

	console.log(params);

	var iframe = document.getElementById("game-frame");

	if (iframe) {
		var iframeContent = (iframe.contentWindow || iframe.contentDocument);

		iframeContent.Start();

		document.getElementById("start_button").disabled = true;
		console.log('Starting the next problem!');
	}
}


function StartExample()
{

	var iframe = document.getElementById("example-frame");

	if (iframe) {
		var iframeContent = (iframe.contentWindow || iframe.contentDocument);

		iframeContent.Start();

		document.getElementById("start_example_button").disabled = true;

	}
}

function UnlockIns2Continue()
{
	document.getElementById("continue_ins2").disabled = false;
}

function EndTrial()
{
	var html = 'Click <span style="font-weight: bold">Continue</span> to move onto the next device';
	$('#lower-prompt-text').html(html);

	SubmitResponse();	
	//document.getElementById("submit_button").disabled = false;
}

function SubmitResponse()
{
	var iframe = document.getElementById("game-frame");

	if (iframe) {
		var iframeContent = (iframe.contentWindow || iframe.contentDocument);

		iframeContent.FinaliseResponse();//RevealNetwork();
		var score = iframeContent.score;
		var bonusScore = iframeContent.bonusScore;
		var n_remaining_ints = iframeContent.n_remaining_ints;

		total_score += score;
		total_bonus += bonusScore;
		var trial_summary = [trial_counter,
		trial_order[trial_counter-1], N, n_remaining_ints,
		score, total_score, bonusScore, total_bonus];

		

		//Store data from iframe
		event_data[trial_counter-1] = iframeContent.data;
		belief_data[trial_counter-1] = iframeContent.belief_trajectory;
		trial_data[trial_counter-1] = trial_summary;

		console.log('trial', trial_counter, 'score', score, 'total_score', total_score,
			'bonus', bonusScore, 'total_bonus', total_bonus,
			'event_data', event_data,
			'belief_data', belief_data,
			'trial_data', trial_data,
			'trial_counter', trial_counter);

	} else {

		console.log('no iframe found upon submit!');

	}

	

	//document.getElementById("submit_button").disabled = true;
	document.getElementById("trial_next_button").disabled = false;

}


function FinishExperiment()
{
	var feedback_string = $('textarea[name = feedback]').val();

	var sex = $('input[name=sex]:checked').val();
	var age = $('input[name=age]').val();
	
	subject_data = [upi,
	sex,
	age,
	learn_cond,
	delay_cond,
	total_score,
	total_bonus,
	];

	event_string = '';
	belief_string = '';
	trial_string = '';

	//Loop over trials
	for (var i = 0; i < event_data.length; i++)
	{
		trial_string+= trial_data[i].toString() + '\n';

		//Loop over nodes
		for (var j = 0; j < event_data[i].eventDelays.length; j++) {
			//events within a trial/node
			for (var k=0; k<event_data[i].eventDelays[j].length; k++)
			{
				event_string+= trial_data[i].toString() + ',' + (j+1).toString() + ',' + 
				event_data[i].eventTimings[j][k].toString() + ',' +
				event_data[i].eventTypes[j][k].toString() + ',' +
				event_data[i].eventOrigins[j][k].toString() + ',' +
				event_data[i].eventDelays[j][k].toString() + '\n';
			}
			
		}
		
		//Loop over changes of belief
		for (var j = 0; j < belief_data[i].length; j++) { 
			
			var tmp_string = '';
			
			//loop over the reported network
			for (var k = 0; k < max_N; k++)//belief_data[i][j][0].length
			{
				tmp_string += belief_data[i][j][0][k].toString() + ',' ;
			}
			tmp_string=tmp_string.substring(0, tmp_string.length-1);//Removes the final ","

			belief_string+= trial_data[i].toString() + ',' + belief_data[i][j][1] + ',' + tmp_string + '\n';
		}
	}
	event_string.substring(0, event_string.length-1);//Removes the final "\n"
	belief_string.substring(0, belief_string.length-1);
	trial_string.substring(0, trial_string.length-1);
	subject_string = subject_data.toString();


	filename = subject_data.join('');
	SaveData(event_string, belief_string, trial_string, subject_string, feedback_string, filename);

}

function ipcheck () 
{
	console.log('test');

	jQuery.ajax({
		url: 'php/check_id.php',
		type:'POST',
		success:function(data)
		{
			if (data==1)
			{
				alert('Unfortunately, you cannot do this experiment because you (or someone in your household) have participated in this experiment or a similar experiment before.');
			} else if (data==0)
			{
				document.getElementById('i-agree').disabled = false;
			} else {
				alert('answer was not 1 or 0!');
			}
			
		},
		error:function()
		{
			alert('failed to connect to ip');
		}
		//var result_str:String = event.target.data.toString();
 		//id_check = Number(result_str);		
 		//trace(id_check);

	})
}


function SaveData(event_string, belief_string, trial_string, subject_data, feedback_data, filename)
{
	//WORKING POSTING TO SQL DATABASE (ONLY WORKS WHEN THE EXP IS ONLINE)
	jQuery.ajax({
		url:  "./php/exp1.php",
		type:'POST',
		data:{
			events:event_string,
			beliefs:belief_string,
			trials:trial_string,
			extras:subject_data,
			feedback:feedback_data
		},
		success:function(data){
			console.log('AJAX success');
			//alert('data saved!');

			ClickContinue("demographics", "thank-you");
		},
		error: function(err){
			
			jQuery('.errorText').fadeIn();

			// console.log('save function ',
			// event_string, event_data[0], event_data[1]);//belief_data, trial_data,
			//subject_data, feedback_data);

			alert('Oops there was a problem linking with our database.\n\nPlease email the experimenter at neil.bramley@ucl.ac.uk to ensure you get paid!');

			// //Save locally with this
			// csvContent = "data:text/csv;charset=utf-8," + event_string;
			// var encodedUri = encodeURI(csvContent);
			// var link = document.createElement("a");
			// link.setAttribute("href", encodedUri);
			// link.setAttribute("download", "events" + filename + ".csv");
			// document.body.appendChild(link); // Required for FF
			// link.click();

			// csvContent = "data:text/csv;charset=utf-8," + belief_string;
			// var encodedUri = encodeURI(csvContent);
			// var link = document.createElement("a");
			// link.setAttribute("href", encodedUri);
			// link.setAttribute("download", "beliefs" + filename + ".csv");
			// document.body.appendChild(link); // Required for FF
			// link.click();

			// csvContent = "data:text/csv;charset=utf-8," + trial_string;
			// var encodedUri = encodeURI(csvContent);
			// var link = document.createElement("a");
			// link.setAttribute("href", encodedUri);
			// link.setAttribute("download", "trials" + filename + ".csv");
			// document.body.appendChild(link); // Required for FF
			// link.click();

			
		}
	});


}


//DEMOGRAPHICS CHECKING  checks whether all questions were answered
$('.demoQ').change(function () {
	if ($('input[name=sex]:checked').length > 0 &&
		$('input[name=age]').val() != "")
	{
		$('#exp_finish').prop('disabled', false)
	}else{
		$('#exp_finish').prop('disabled', true)
	}
});

$('.numberQ').change(function (e) {    
	if($(e.target).val() > 100){
		$(e.target).val(100)
	}
});





////////////////////////
//Run at start
////////////////////////


console.log('Experiment js loaded');
ipcheck();
