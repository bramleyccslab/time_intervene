// Javascript for demo of intervention and time

/////////////
//Non-user-definable things
/////////////

//var N = 5 //How many nodes
//var density = .1 //What proportion of the nodes should be connected on average if drawing connections at random.
////var locMode = 'even'; //'random'; //Or else equal = equally spaced

var locations = [];
var edges = [];
var nodes = [];
var activeNodes = [];
var activations = [];

var n_background = 0;

var nodeRadius = 30;//25; //How many pixels big should the nodes be?
var arrowAngle = 15 * (Math.PI / 180);
var baseColor = 0xB3B3B3;
var blockedColor = 0x000000;
var activeColor = 0xFFD000;

var arrowColor = 0x000000;
var revealColor = 0x999999;//0xB3B3B3;

var onFor = 250; //How long should each node light up?



var ongoing = []; //Makes sure there's only one ongoing event per edge

var propEvs = []; //Keep a list of all the propagation event timers
var deactEvs = [];

var score = 0;//Just for counting the score at the end
var bonusScore = 0;


var data = [];
data.eventTimings = [];
data.eventOrigins = [];
data.eventDelays = [];
data.eventTypes = [];

//Set up stage on the canvas
//var stage = new Stage("c");



///////////
//Functions
///////////

function Start() {

	//Set up stage on the canvas
	stage = new Stage("c");
	s = new Sprite();
	stage.addChild(s);


	c.width = 600;
	c.height = 400;
	//console.log(parent, parent.params);

	//Grab the parameters
	
	N = parent.params.N;
	max_N = parent.params.max_N;
	network = parent.params.network;
	node_position = parent.node_position;
	mode = parent.params.mode;
	learn_cond = parent.learn_cond;
	delay_cond = parent.delay_cond;
	locMode = parent.params.locMode;
	contingency = parent.params.contingency;
	timeout = sec = parent.params.timeout;
	which_subject_background = parent.params.which_subject_background;
	background = parent.params.background;
	delay_mu = parent.params.delay_mu;
	delay_alpha = parent.params.delay_alpha;
	delay_beta = delay_mu / delay_alpha;
	n_ints = parent.params.n_ints;
	n_remaining_ints = n_ints;

	//time_space_cor = parent.params.time_space_cor;

	//Bonus timing
	bonusTime = Math.floor(Math.random()*timeout)*1000;
	console.log('bonus paid at', bonusTime);
	setTimeout(ScoreBonus, bonusTime);
	

	//Ongoing keeps track of which causal pathways ActivateRevealButton
	//currently working it should be an array of dimension N*N
	//belief_network keeps track of participants' belief about the network
	ongoing = new Array();
	belief_network = new Array();
	toggleBackward = new Array();
	for (var i = 0; i < N; i++)
	{
		ongoing[i] = Array(N+1).join('0').split('').map(parseFloat)
		toggleBackward[i] = Array(N+1).join('0').split('').map(parseFloat)
		
		for (var j = 0; j < N; j++)
		{
			toggleBackward[i][j] = Math.round(Math.random());
		}
	}

	for (var i = 0; i < max_N; i++)
	{
		belief_network[i] = Array(max_N+1).join('0').split('').map(parseFloat)
	}


	//If we want to have consistent delays for a given link but variability between links:
	if (delay_cond=='rel_within_var_between')
	{
		vbrw_delays = [];

		for (var i = 0; i < N; i++)
		{
			vbrw_delays[i] = [];
			for (var j = 0; j < i; j++)
			{
				var lProd = Math.log(Math.random());
				for (var k = 1; k < delay_alpha; k++) {
					lProd = lProd + Math.log(Math.random());
				}

				vbrw_delays[i][j] = -delay_beta * lProd;
				vbrw_delays[j][i] = -delay_beta * lProd;
			}
		}
		console.log('vbrw_delays',vbrw_delays, delay_alpha);
	}
	//console.log('Params', N, max_N, network, node_position, belief_network, mode, locMode, contingency,
	//	timeout, which_subject_background, background,
	//	delay_mu, delay_alpha, delay_beta, ongoing);

	

	////////////////////////
	//Initialise the canvas
	////////////////////////

	pixel_ratio = window.devicePixelRatio;
	//console.log('pixel_ratio', pixel_ratio);

	f1 = new TextFormat("Arial Black", 15 * pixel_ratio, 0x000000, true, false, "right");

	//console.log('c', c, '\n s', s, '\n stage', stage, stage.canvas);
	//console.log(c.width, stage.stageWidth, stage.canvas.width);

	var d = new Date();
	startTime = d.getTime();
	//console.log('data', data);
	
	belief_trajectory = [];

	for (var i = 0; i < N; i++)
	{
		data.eventTimings[i] = [];
		data.eventOrigins[i] = [];
		data.eventDelays[i] = [];
		data.eventTypes[i] = [];

	}
	/////////////////////////////
	//Initialise and draw network
	/////////////////////////////

	//Creates network (an adjacency matrix)
	//GenerateNetwork(N, density);

	//Creates locations, an array of x and y locations for the nodes
	SelectLocations(N, [0, c.width * pixel_ratio],
		[0, c.height * pixel_ratio], locMode, nodeRadius);



	//Creates nodes and activeNodes (arrays of node sprites)
	//And listenNodes (boolean tracking the event listeners again)
	//console.log('locations at start', locations, node_position)
	
	DrawNetwork(locations, node_position, nodeRadius * pixel_ratio, arrowAngle, arrowColor, revealColor);

	DrawNodes(locations, node_position, nodeRadius * pixel_ratio, baseColor, activeColor);

	//Creates
	GenerateBaselineActivations(which_subject_background, background, N);

	BaselineActivations(activations);

	//console.log(which_subject_background, N, timeout, background, delay_mu, delay_alpha);

	CreateInterfaceObjects();

	
	//Creates edges (array of edge sprites)
	//...and listenEdges (boolean tracking if they have event listeners attached)
	if (mode == 'practice') {
		TurnOnEdges(network, edgesRevealed);
		//console.log('drawing network at start');
	}

	//Start out with a belief that there are no connections....
	var curNetwork = [];
	for (var i = 0;i<belief_network.length; ++i)
	{
		curNetwork[i] = belief_network[i].slice(0);
	}
	belief_trajectory.push([curNetwork, 0]);

	StartTimer();
	//StartClockTimer();


}


function Stop() {
	//console.log('Stop activations!');

	//Stop any timers
	//console.log('propEvs', propEvs, 'deactEvs', deactEvs);
	for (var i = 0; i < 9999; i++) {
		window.clearInterval(i);
		window.clearTimeout(i);
	}

	//Stop the activations
	for (var i = 0; i < activations.length; i++) {
		for (var j = 0; j < activations[i].length; j++) {
			clearTimeout(activationTimeouts[i][j]);
		}

		// for (j = 0; j < edges[i].length; j++) {
		// 	// if (listenEdges[i][j] == true) {
		// 	// }

		// 	if (i != j) {
		// 		stage.removeChild(edges[i][j]);
		// 	}

		// }

		//Remove all the event listeners
		if (listenNodes[i] == true) {

			blockedNodes[i].removeEventListener(MouseEvent.CLICK, StopBlock, false);
			blockedNodes[i].removeEventListener(MouseEvent.RIGHT_CLICK, StopBlock, false);
			nodes[i].removeEventListener(MouseEvent.RIGHT_CLICK, StartBlock, false);
			nodes[i].removeEventListener(MouseEvent.CLICK, ActivationEvent, false);
			activeNodes[i].removeEventListener(MouseEvent.RIGHT_CLICK, StartBlock, false);
			listenNodes[i] = false;
			console.log('remove node listeners??', i)
		}

		activeNodes[i].visible = false;
		blockedNodes[i].visible = false;
		//Remove sprites
		// stage.removeChild(nodes[i]);
		// stage.removeChild(blockedNodes[i]);
		// stage.removeChild(activeNodes[i]);
	}

	ActivateSubmitButton();
	
	//Remove the timer
	// stage.removeChild(timer);
	// stage.removeChild(counter);

	timer.text = ' ';
	//nodes = blockedNodes = activeNodes = edges = [];

	//Store data!
	//parent.dataBox.text = data.toString();
	//console.log('data', data);

}

function FinaliseResponse ()
{
	
	for (var i = 0; i < N; i++)
	{
		for (var j = 0; j < N; j++) {
			if (j<i)
			{
				if (network[i][j]==belief_network[i][j] & network[j][i]==belief_network[j][i])
				{
					score = score+1;
				}
			}
			
		}
		
	}

	//Reveal the true network
	RevealNetwork();

	//Clear up any other remaining stuff
	//nodes = blockedNodes = activeNodes = edges = [];
	//console.log('network', network, 'nodes', nodes, 'edges', edges);

	//Release the continue button
	//parent.endTrial();
}

function Pause() {

	//Stop any timers
	//console.log('propEvs', propEvs, 'deactEvs', deactEvs);
	for (var i = 0; i < 9999; i++) {
		window.clearInterval(i);
		window.clearTimeout(i);
	}

	//Clear out activations
	for (var i = 0; i < activations.length; i++) {
		for (var j = 0; j < activations[i].length; j++) {
			clearTimeout(activationTimeouts[i][j]);
		}

		//Remove all the event listeners
		if (listenNodes[i] == true) {

			blockedNodes[i].removeEventListener(MouseEvent.CLICK, StopBlock, false);
			blockedNodes[i].removeEventListener(MouseEvent.RIGHT_CLICK, StopBlock, false);
			nodes[i].removeEventListener(MouseEvent.RIGHT_CLICK, StartBlock, false);
			nodes[i].removeEventListener(MouseEvent.CLICK, ActivationEvent, false);

			activeNodes[i].removeEventListener(MouseEvent.RIGHT_CLICK, StartBlock, false);

			listenNodes[i] = false;
		}
	}
	activations = [];
}

function Resume() {

	//console.log(nodes);

	//Sets up some new baseline activations
	GenerateBaselineActivations(which_subject_background, background, N);

	for (var i = 0; i < nodes.length; i++) {

		//console.log('reinstate listening', listenNodes);
		//Add back all the event listeners
		if (learn_cond == 'active' & listenNodes[i] == false) {
			blockedNodes[i].addEventListener(MouseEvent.CLICK, StopBlock, false);
			blockedNodes[i].addEventListener(MouseEvent.RIGHT_CLICK, StopBlock, false);
			nodes[i].addEventListener(MouseEvent.RIGHT_CLICK, StartBlock, false);
			nodes[i].addEventListener(MouseEvent.CLICK, ActivationEvent, false);
			activeNodes[i].addEventListener(MouseEvent.RIGHT_CLICK, StartBlock, false);

			listenNodes[i] = true;
		}
	}

	StartTimer();
}



function ActivationEvent(evt) {
	console.log('clicked on node', evt.target.id);
	whichBlocked[evt.target.id] = false;
	ActivateNode([evt.target.id, 'a',0]);

	if (learn_cond=='active')
	{
		n_remaining_ints =n_remaining_ints -1;
		//console.log('did an intervention');
		counter.text = 'Tests remaining: ' + n_remaining_ints;
		counter.width = counter.textWidth;
		counter.height = counter.textHeight;

		if (n_remaining_ints==0)
		{
			console.log('out of tests!');

			for (i = 0; i < nodes.length; i++) {

				blockedNodes[i].removeEventListener(MouseEvent.CLICK, StopBlock, false);
				blockedNodes[i].removeEventListener(MouseEvent.RIGHT_CLICK, StopBlock, false);
				nodes[i].removeEventListener(MouseEvent.RIGHT_CLICK, StartBlock, false);
				nodes[i].removeEventListener(MouseEvent.CLICK, ActivationEvent, false);
				activeNodes[i].removeEventListener(MouseEvent.RIGHT_CLICK, StartBlock, false);
			}

		}
	}
}

function StartBlock(evt) {
	console.log('start block', evt.target.id);

	activeNodes[evt.target.id].visible = false;

	BlockNode(evt.target.id);
}

function StopBlock(evt) {
	console.log('stop block', evt.target.id);
	UnblockNode(evt.target.id);
}

function ToggleEdge(evt) {
	
	console.log('toggle belief',
		evt.target.id1,
		evt.target.id2 );

	var polygon = [[evt.target.graphics._points[2],evt.target.graphics._points[3]],
	[evt.target.graphics._points[4],evt.target.graphics._points[5]],
	[evt.target.graphics._points[6],evt.target.graphics._points[7]],
	[evt.target.graphics._points[8],evt.target.graphics._points[9]]];

	if (inside([evt.target.mouseX, evt.target.mouseY], polygon))
	{
		UpdateBelief(evt.target.id1, evt.target.id2);
		//console.log('in!');
	} else {
		//console.log('out!');

		//If the click was within the sprite but not in the polygon,
		//check all other edge polygons to see if it was actually in one of them.
		for (var i = 0; i < locations.length; i++) {
			for (var j = 0; j < i; j++) {

				console.log('bubble toggling', i,j);
				BubbleToggle(edgeSpaces[i][j]);
			}
		}
	}
}

function BubbleToggle(target) {
	//Toggles edges behind if the actually clicked on edge space sprite
	//was on top but outside the polygon


	var polygon = [[target.graphics._points[2],target.graphics._points[3]],
	[target.graphics._points[4],target.graphics._points[5]],
	[target.graphics._points[6],target.graphics._points[7]],
	[target.graphics._points[8],target.graphics._points[9]]];

	if (inside([target.mouseX, target.mouseY], polygon))
	{
		//console.log('bubbled in!', target.id1, target.id2);
		UpdateBelief(target.id1, target.id2);
		
	}
}

function BlockNode(id) {
	blockedNodes[id].visible = true;
	whichBlocked[id] = true;

	var d = new Date();
	var curTime = d.getTime();
	data.eventTimings[id].push(curTime - startTime);
	data.eventOrigins[id].push(0);
	data.eventDelays[id].push(0);
	data.eventTypes[id].push('block');
}

function UnblockNode(id) {
	blockedNodes[id].visible = false;
	whichBlocked[id] = false;

	var d = new Date();
	var curTime = d.getTime();
	data.eventTimings[id].push(curTime - startTime);
	data.eventOrigins[id].push(0);
	data.eventDelays[id].push(0);
	data.eventTypes[id].push('unblock');
}

function UpdateBelief(id1, id2)
{
	
	var curPos = (belief_network[id1][id2] & !belief_network[id2][id1]) +
	2*(belief_network[id1][id2] & belief_network[id2][id1]) +
	3*(!belief_network[id1][id2] & belief_network[id2][id1]);

	var newPos = undefined;

	if (toggleBackward[id1][id2]==0)
	{
		newPos = curPos + 1;
		if (newPos>3)
		{
			newPos = 0;
		}

	} else {
		newPos = curPos -1;
		if (newPos<0)
		{
			newPos = 3;
		}
	}

	if (newPos == 0)
	{
		belief_network[id1][id2]=0;
		belief_network[id2][id1]=0;
	} else if (newPos == 1)
	{
		belief_network[id1][id2]=1;
		belief_network[id2][id1]=0;
	} else if (newPos == 2)
	{
		belief_network[id1][id2]=1;
		belief_network[id2][id1]=1;
	} else if (newPos == 3)
	{
		belief_network[id1][id2]=0;
		belief_network[id2][id1]=1;
	}

	TurnOnEdges(belief_network, edges);
	
	//console.log(belief_network, 'oldPos', curPos, 'newPos', newPos,
	//	'id1',id1,'id2',id2, toggleBackward[id1][id2]);

	//Check if it is different from how it was last time it was confirmed
	
	var prev_judg = belief_trajectory[belief_trajectory.length-1][0];
	var prev_judg_diff = false;
	for (var i = 0; i < N; i++)
	{
		for (var j = 0; j < i; j++)
		{
			console.log('ij pre', i,j, prev_judg[i][j]!=belief_network[i][j], prev_judg[j][i]!=belief_network[j][i]);

			if (prev_judg[i][j]!=belief_network[i][j] | prev_judg[j][i]!=belief_network[j][i])
			{
				prev_judg_diff = true;
			}	
		}
	}
	console.log('prev_judg_diff', prev_judg_diff)

	if (prev_judg_diff == true)
	{
		confirmBtn.visible = true;
	} else {
		confirmBtn.visible = false;
	}

	console.log('hi', belief_trajectory, belief_network);

	id1 = undefined;
	id2 = undefined;
}

function ConfirmSelection()
{
	confirmBtn.visible = false;

	var d = new Date();
	var curTime = d.getTime();
	var curNetwork = [];
	for (var i = 0;i<belief_network.length; ++i)
	{
		curNetwork[i] = belief_network[i].slice(0);
	}
	
	belief_trajectory.push([curNetwork, curTime-startTime]);
}

function ActivateNode(par) {

	//TODO add an origin variable
	//console.log('par',par)
	var id = par[0];
	var origin = par[1];
	var prevDelay = par[2];

	var a = activeNodes[id];

	var d = new Date();
	var curTime = d.getTime();

	//console.log('origin',origin,'id',id);
	if (origin!='a' & origin!='b')
	{
		ongoing[origin][id] = 0;
		data.eventTypes[id].push('effect');
		data.eventOrigins[id].push(origin+1);
	} else{
		data.eventTypes[id].push(origin);
		data.eventOrigins[id].push(0);

	}
	

	//console.log(data);
	data.eventTimings[id].push(curTime - startTime);
	
	data.eventDelays[id].push(prevDelay);


	

	// If its not blocked
	if (whichBlocked[id] == false) {
		//Activate the node?
		a.visible = true;

		if(origin=='a')// | origin=='b')
		{
			activeNodesPlusses[id].visible = true;
		}

		//Countdown until deactivation
		deactEv = setTimeout(DeactivateNode, onFor, [a.id]);
		deactEvs.push(deactEv);


		//Loop over potential propagations
		for (var i = 0; i < network.length; i++) {

			//If there is a connection
			if (network[a.id][i] == 1) {
				//console.log('a.id', a.id, 'i', i, network, contingency)
				//...and if the connection works
				if (Math.random() < contingency) {
					//And if the pathway is not already active
					//if (ongoing[a.id][i] != 1) {
					
						//Generate a delay
						//Generates an Erlang distributed number with shape alpha and rate beta]
						//Gammas were too hard to generate!
						if (delay_cond!= 'rel_within_var_between')
						{

							var lProd = Math.log(Math.random());
							for (j = 1; j < delay_alpha; j++) {
								lProd = lProd + Math.log(Math.random());
							}

							delay = -delay_beta * lProd;

						} else {
							delay = vbrw_delays[a.id][i];
						}


						console.log('delay_cond', delay_cond, 'propagation from ', a.id, 'to', i, 'delay', delay);

						var propEv = setTimeout(ActivateNode, delay, [i, id, Math.round(delay)]);

						ongoing[a.id][i] = 1;

						//Display the ongoing processes
						
						// var ongoingflat = ongoing.reduce(function(a, b) {
						// 	return a.concat(b);
						// }, []);
						// var total = ongoingflat.reduce(function(a, b) {
						// 	return a + b;
						// });
						// counter.text = total;

						setTimeout(turnOffProcess, delay, a.id, i)

						propEvs.push(propEv); //Keep hold of all active propagation timeouts
					}


				//}
			}

		}
	} else {
		//console.log('activation at ', id, 'blocked');
	}

}

function DeactivateNode(params) {

	var a = activeNodes[params[0]];
	//console.log('deactivating', a.id);
	var plus = activeNodesPlusses[params[0]];
	plus.visible = false;
	a.visible = false;
}

function BaselineActivations(activations) {
	activationTimeouts = [];
	for (var i = 0; i < activations.length; i++) {
		activationTimeouts[i] = [];
		for (var j = 0; j < activations[i].length; j++) {

			//console.log(i, j, 'a', a, a.x, a.y, activations[i][j], a.visible);

			activationTimeouts[i][j] = setTimeout(ActivateNode, activations[i][j], [i, 'b', 0]);
			// setTimeout(function() {
			// 	ActivateNode(activeNodes, i, 1000);
			// }, activations[i][j])
			//console.log(i, j, activationTimeouts[i][j])
		}

	}
}

function GenerateBaselineActivations(which_subject_background, rate, N) {

	activations = [];
	for (var i = 0; i < N; i++) {
		activations[i] = []
	}

	if (which_subject_background == 'roots') {
		for (var i = 0; i < N; i++) {
			aRoot = true;
			for (var j = 0; j < N; j++) {
				if (network[j][i] == 1) {
					aRoot = false;
				}
			}

			if (aRoot == true) {
				//activations[i].push(0);//An initial activation
				proposal = -Math.log(1 - Math.random()) / (rate / 1000);

				while (proposal < (timeout * 1000)) {
					activations[i].push(proposal);
					proposal = proposal - Math.log(1 - Math.random()) / (rate / 1000);
				}
			}
		}

	} else if (which_subject_background == 'all') {
		for (var i = 0; i < N; i++) {
			//activations[i].push(0);//An initial activation
			proposal = -Math.log(1 - Math.random()) / (rate / 1000);

			while (proposal < (timeout * 1000)) {
				activations[i].push(proposal);
				proposal = proposal - Math.log(1 - Math.random()) / (rate / 1000);
			}
		}
	} else if (which_subject_background=='fixed')
	{
		for (var i = 0; i<n_background; ++i)
		{
			var tmp = Math.floor(Math.random()*timeout*1000);
			activations[Math.floor(Math.random()*N)].push(tmp);
		}
	}

	// for (var i = 0; i < N; i++) {

	// 	//console.log('hihi', -Math.log(1 - Math.random()) / (rate / 1000));
	// 	proposal = -Math.log(1 - Math.random()) / (rate / 1000);

	// 	while (proposal < (timeout * 1000)) {
	// 		activations[i].push(proposal);
	// 		proposal = proposal - Math.log(1 - Math.random()) / (rate / 1000);
	// 	}
	// }

	console.log('baseline activations', activations);

	//return activations;
}


//Draws N nodes randomly on the canvas
//Stores their locations/ids somehow
function DrawNodes(locations, node_pos, nodeRadius, baseColor, activeColor, blockedColor) {

	nodes = [];
	nodeSockets = [];
	activeNodes = [];
	activeNodesPlusses = [];
	blockedNodes = [];
	listenNodes = [];
	whichBlocked = [];

	var f2 = new TextFormat("Arial Black", 30 * pixel_ratio, 0x000000, true, false);

	for (var i = 0; i < locations.length; i++) {


		n = new Sprite();
		n.graphics.beginFill(baseColor, 1);
		n.graphics.drawCircle(0, 0, nodeRadius); //(Math.random()*c.width, Math.random()*c.height
		n.graphics.endFill();
		stage.addChild(n);



		b = new Sprite();
		b.graphics.beginFill(blockedColor, 1);
		b.graphics.drawCircle(0, 0, nodeRadius); //(Math.random()*c.width, Math.random()*c.height
		b.graphics.endFill();


		stage.addChild(b);

		a = new Sprite();
		a.graphics.beginFill(activeColor, 1);
		a.graphics.drawCircle(0, 0, nodeRadius); //(Math.random()*c.width, Math.random()*c.height
		a.graphics.endFill();
		a.id = i;
		stage.addChild(a);

		s = new Sprite();
		s.graphics.lineStyle(2, 0x000000, 1);
		s.graphics.drawCircle(0, 0, nodeRadius); //(Math.random()*c.width, Math.random()*c.height
		//s.graphics.endFill();
		stage.addChild(s);

		//console.log(b)
		
		aPlus = new TextField();
		aPlus.selectable = false; // default is true
		aPlus.setTextFormat(f2);
		aPlus.text = '+';
		aPlus.width = aPlus.textWidth;
		aPlus.height = aPlus.textHeight;
		stage.addChild(aPlus);

		n.x = b.x = a.x = s.x = locations[node_pos[i]][0];
		n.y = b.y = a.y = s.y = locations[node_pos[i]][1];
		aPlus.x = locations[node_pos[i]][0] - .5*aPlus.width;
		aPlus.y = locations[node_pos[i]][1] - .5*aPlus.height;
		n.id = b.id = a.id = aPlus.id = i;
		a.visible = b.visible = aPlus.visible = false;//

		//console.log(b)
		


		if (learn_cond == 'active') {
			n.addEventListener(MouseEvent.CLICK, ActivationEvent, false);
			listenNodes.push(true);	

		} else if (learn_cond== 'active_block')
		{
			n.addEventListener(MouseEvent.RIGHT_CLICK, StartBlock, false);
			a.addEventListener(MouseEvent.RIGHT_CLICK, StartBlock, false);
			b.addEventListener(MouseEvent.CLICK, StopBlock, false);
			b.addEventListener(MouseEvent.RIGHT_CLICK, StopBlock, false);

			listenNodes.push(true);	
		} else {
			listenNodes.push(false);
		}

		whichBlocked.push(false);

		nodes.push(n);
		blockedNodes.push(b);
		activeNodes.push(a);
		activeNodesPlusses.push(aPlus);
	}

	console.log(listenNodes);
	//return [nodes, activeNodes];
}


function DrawNetwork(locations, node_pos, d, t, colorB, colorR) {
	edges = [];
	edgesRevealed = [];
	edgeSpaces = [];
	edgeSpacesRight =[];
	edgeSpacesWrong = [];

	for (var i = 0; i < locations.length; i++) {
		edges[i] = [];
		edgesRevealed[i] = [];
		edgeSpaces[i] = [];
		edgeSpacesRight[i] =[];
		edgeSpacesWrong[i] = [];

		for (var j = 0; j < locations.length; j++) {

			

			if (i != j) {

				e = new Sprite();//edge
				eR = new Sprite();//edge revealed
				eS = new Sprite();//edge space
				eSR = new Sprite();
				eSW = new Sprite();

				theta = Math.atan((locations[node_pos[j]][1] - locations[node_pos[i]][1]) /
					(locations[node_pos[j]][0] - locations[node_pos[i]][0]));
				//console.log(theta)

				var w = (locations[node_pos[j]][0] - locations[node_pos[i]][0])
				var h = (locations[node_pos[j]][1] - locations[node_pos[i]][1])

				var x0 = d * Math.abs(Math.cos(theta)) * sign(w);//Math.sign(w);
				var y0 = d * Math.abs(Math.sin(theta)) * sign(h);//Math.sign(h);

				var x1 = w - d * Math.abs(Math.cos(theta)) * sign(w);//Math.sign(w);
				var y1 = h - d * Math.abs(Math.sin(theta)) * sign(h);//Math.sign(h);

				// var x1 = locations[j][0] - locations[i][0];
				// var y1 = locations[j][1] - locations[i][1];
				var l = Math.sqrt(Math.pow(x1 - x0, 2) + Math.pow(y1 - y0, 2));
				var arh = [
				[],
				[]
				];


				arh[0][0] = x1 - [(x1 - x0) * Math.cos(t) - (y1 - y0) * Math.sin(t)] * (d / l)
				arh[0][1] = y1 - [(y1 - y0) * Math.cos(t) + (x1 - x0) * Math.sin(t)] * (d / l)

				arh[1][0] = x1 - [(x1 - x0) * Math.cos(t) + (y1 - y0) * Math.sin(t)] * (d / l)
				arh[1][1] = y1 - [(y1 - y0) * Math.cos(t) - (x1 - x0) * Math.sin(t)] * (d / l)

				//console.log('i', i, 'j', j, 'signs', Math.sign(w), Math.sign(h), 'cos', Math.cos(theta), 'sin', Math.sin(theta), 'coords', x0, y0, x1, y1, 'l', l, [x1, y1, arh[0][0], arh[0][1], arh[1][0], arh[1][1]]);


				e.graphics.lineStyle(3, colorB);

				e.graphics.moveTo(x0, y0);

				e.graphics.lineTo(x1, y1);

				//  "buffered" triangle
				e.graphics.beginFill(colorB);
				e.graphics.drawTriangles([x1, y1, arh[0][0], arh[0][1], arh[1][0], arh[1][1]], [0, 1, 2]);
				e.graphics.endFill();

				//And revealed network
				arh[0][0] = x1 - [(x1 - x0) * Math.cos(2*t) - (y1 - y0) * Math.sin(2*t)] * (d / l) * 1.5;
				arh[0][1] = y1 - [(y1 - y0) * Math.cos(2*t) + (x1 - x0) * Math.sin(2*t)] * (d / l) * 1.5;

				arh[1][0] = x1 - [(x1 - x0) * Math.cos(2*t) + (y1 - y0) * Math.sin(2*t)] * (d / l) * 1.5;
				arh[1][1] = y1 - [(y1 - y0) * Math.cos(2*t) - (x1 - x0) * Math.sin(2*t)] * (d / l) * 1.5;

				eR.graphics.lineStyle(3, colorR);

				eR.graphics.moveTo(x0, y0);

				eR.graphics.lineTo(x1, y1);

				//  "buffered" triangle
				eR.graphics.beginFill(colorR);
				eR.graphics.drawTriangles([x1, y1, arh[0][0], arh[0][1], arh[1][0], arh[1][1]], [0, 1, 2]);
				eR.graphics.endFill();

				//eS = edge space, draw a transparent rectangle on top of the arrow location
				if (i>j)
				{
					

					var eslim = new Array();
					eslim[0] = x0 - [ - (y1 - y0) * Math.sin(t)] * (3*d / l);
					eslim[1] = y0 - [ + (x1 - x0) * Math.sin(t)] * (3*d / l);

					eslim[2] = x0 - [ + (y1 - y0) * Math.sin(t)] * (3*d / l);
					eslim[3] = y0 - [ - (x1 - x0) * Math.sin(t)] * (3*d / l);

					

					eslim[4] = x1 - [ + (y1 - y0) * Math.sin(t)] * (3*d / l);
					eslim[5] = y1 - [ - (x1 - x0) * Math.sin(t)] * (3*d / l);

					eslim[6] = x1 - [ - (y1 - y0) * Math.sin(t)] * (3*d / l);
					eslim[7] = y1 - [ + (x1 - x0) * Math.sin(t)] * (3*d / l);
					//console.log('eslim', eslim);

					//eS.graphics.lineStyle(nodeRadius, color, .1);
					eS.graphics.beginFill(colorB, .1);
					eS.graphics.moveTo(eslim[0], eslim[1]);
					eS.graphics.lineTo(eslim[2], eslim[3]);
					eS.graphics.lineTo(eslim[4], eslim[5]);
					eS.graphics.lineTo(eslim[6], eslim[7]);

					//eS.graphics.drawCircle(x0 + (x1-x0)/2, y0 + (y1-y0)/2, 50);//Testing clicking
					eS.graphics.endFill();

					eSR.graphics.beginFill(0x00FF00,.3);
					eSR.graphics.moveTo(eslim[0], eslim[1]);
					eSR.graphics.lineTo(eslim[2], eslim[3]);
					eSR.graphics.lineTo(eslim[4], eslim[5]);
					eSR.graphics.lineTo(eslim[6], eslim[7]);

					//eS.graphics.drawCircle(x0 + (x1-x0)/2, y0 + (y1-y0)/2, 50);//Testing clicking
					eSR.graphics.endFill();

					eSW.graphics.beginFill(0xFF0000,.3);
					eSW.graphics.moveTo(eslim[0], eslim[1]);
					eSW.graphics.lineTo(eslim[2], eslim[3]);
					eSW.graphics.lineTo(eslim[4], eslim[5]);
					eSW.graphics.lineTo(eslim[6], eslim[7]);

					//eSW.graphics.drawCircle(x0 + (x1-x0)/2, y0 + (y1-y0)/2, 50);//Testing clicking
					eSW.graphics.endFill();


				}

				//Draw a triangle half a node-width from the head end
				//Create all the shapes as sprites but only make some visible (i.e. move this to draw nodes)
				

				//listenEdges[i][j] = true;??
				stage.addChild(eR);
				stage.addChild(e);
				stage.addChild(eS);
				stage.addChild(eSR);
				stage.addChild(eSW);

				e.x = locations[node_pos[i]][0];
				e.y = locations[node_pos[i]][1];
				
				eR.x = locations[node_pos[i]][0];
				eR.y = locations[node_pos[i]][1];

				eS.x = locations[node_pos[i]][0];
				eS.y = locations[node_pos[i]][1];

				eS.id1 = i;
				eS.id2 = j;

				eSR.x = locations[node_pos[i]][0];
				eSR.y = locations[node_pos[i]][1];

				eSR.id1 = i;
				eSR.id2 = j;

				eSW.x = locations[node_pos[i]][0];
				eSW.y = locations[node_pos[i]][1];

				eSW.id1 = i;
				eSW.id2 = j;

				eSR.visible = false;
				eSW.visible = false;
				
				e.visible = false;
				eR.visible = false;
				
				edges[i][j] = e;
				edgesRevealed[i][j] = eR;
				edgeSpaces[i][j] = eS;
				edgeSpacesRight[i][j] = eSR;
				edgeSpacesWrong[i][j] = eSW;

				edgeSpaces[i][j].addEventListener(MouseEvent.CLICK, ToggleEdge, false);
			}

		}
	}
	//console.log('edgesRevealed', edgesRevealed, colorB, colorR);
	//console.log('edges', edges);
	//return edges;
	console.log('stage', stage)
}


// function GenerateNetwork(N, density) {
// 	network = [];
// 	ongoing = [];
// 	for (var i = 0; i < N; i++) {
// 		network[i] = [];
// 		ongoing[i] = [];
// 		for (var j = 0; j < N; j++) {

// 			ongoing[i][j] = 0; //Tracks ongoing causation
// 			if (Math.random() < density & i != j) {
// 				//console.log(i, j)
// 				network[i][j] = 1;

// 			} else {
// 				network[i][j] = 0;
// 			}

// 		}
// 	}
// 	console.log('network', network);

// 	//return network;
// }


//Select locations for the nodes
function SelectLocations(N, xrange, yrange, locMode, nodeRadius) {

	locations = [];

	if (locMode == 'random') {

		var count = 0;

		while (count < N) {
			var proposal = [Math.round(Math.random() * (xrange[1] - xrange[0]) + xrange[0]),
			Math.round(Math.random() * (yrange[1] - yrange[0]) + yrange[0])
			]

			var pass = 1;
			for (var i = 0; i < (locations.length); i++) {

				//Non overlapping
				if (!(Math.abs(proposal[0] - locations[i][0]) > (nodeRadius * 3) &
					Math.abs(proposal[1] - locations[i][1]) > (nodeRadius * 3))) {
					pass = 0;
			}

		}

			//console.log('pass', pass, 'locations.length', locations.length);

			if (pass == 1) {
				locations.push(proposal);
				count = count + 1;
			} else {
				//console.log('too close', locations, proposal);
			}
		}
	} else if (locMode == 'even') {

		var C = [(xrange[1] - xrange[0]) / 2 + xrange[0], (yrange[1] - yrange[0]) / 2 + yrange[0]];

		var r = Math.min(C[0], C[1]) - 1.5 * pixel_ratio * nodeRadius;
		var increment = (360 / N) * Math.PI / 180;
		var origin = Math.PI;//Math.floor(Math.random()*Math.PI);
		for (var x = 0; x < N; x++) {

			var proposal = [r * Math.sin(increment * x+ origin) + C[0], r * Math.cos(increment * x+ origin) + C[1]];
			//console.log('proposal', x, proposal, increment * x, Math.cos(increment * x), increment * x, Math.sin(increment * x))
			locations.push(proposal)
				//(r×cos(36∘x+18∘),r×sin(36∘x+18∘))
				//with x={0,1,2,3,4,5,6,7,8,9}
			}
		}
	//console.log('locations', xrange, yrange, locations);

	//return locations;
}



function CreateInterfaceObjects() {

	//Interface counter

	counter = new TextField();

		counter.selectable = false; // default is true
		counter.setTextFormat(f1);

		


		if (learn_cond=='active'){
			counter.text = 'Tests remaining: ' + n_remaining_ints;
		} else {
			counter.text = '    ';
		}

		counter.width = counter.textWidth;
		counter.height = counter.textHeight;

		stage.addChild(counter);
		counter.x = pixel_ratio * (c.width - 200);
		counter.y = pixel_ratio * (c.height - 60);
		//console.log('process counter', pixel_ratio, counter.x, counter.y)



	//Clock that is updated with a set interval
	timer = new TextField();

	timer.selectable = false; // default is true
	timer.setTextFormat(f1);
	timer.text = "Time remaining: " + sec;
	timer.width = timer.textWidth;
	timer.height = timer.textHeight;
	stage.addChild(timer);
	timer.x = pixel_ratio * 20;//0;
	timer.y = pixel_ratio * 20;//(c.height - 60);

	confirmBtn = new TextField(); //document.createElement("input");
	//Assign different attributes to the element. 
	confirmBtn.selectable = false;
	confirmBtn.setTextFormat(f1);
	confirmBtn.text = "\n Confirm \n";
	confirmBtn.width = confirmBtn.textWidth;
	confirmBtn.height = confirmBtn.textHeight;
	console.log('confirmBtn', confirmBtn);
	stage.addChild(confirmBtn);
	confirmBtn.background = true;
	confirmBtn.border = true;

	confirmBtn.x = pixel_ratio * c.width/2 - confirmBtn.width/2;//pixel_ratio * (c.width - confirmBtn.textWidth * 2);
	confirmBtn.y = pixel_ratio * c.height/2 - confirmBtn.height/2;//confirmBtn.textHeight * 2;
	confirmBtn.visible = false;
	confirmBtn.addEventListener(MouseEvent.CLICK, ConfirmSelection, false);
}


function turnOffProcess(from, to) {
	

	var ongoingflat = ongoing.reduce(function(a, b) {
		return a.concat(b);
	}, []);

	var total = ongoingflat.reduce(function(a, b) {
		return a + b;
	});

	//counter.text = total;
	//console.log('ended process');
}

function StartTimer() {

	timerObjFunc = setInterval(function() {
		sec = sec - 1;
		timer.text = 'Time remaining: ' + sec.toFixed(0); //pad(++sec % 60);
		timer.width = timer.textWidth;
		if (sec == 0) {
			timer.text = ' ';
			ConfirmSelection();
			Stop();
		}

	}, 1000);
	console.log('starting timer', timer)
}

// function StartClockTimer() {

// 	clockTimerObjFunc = setInterval(function() {
// 		var d = new Date();
// 		var sec = (d.getSeconds() - startTime); //.toFixed(1)
// 		var ms = Math.round((d.getMilliseconds() - startTime) / 100);
// 		clockTimer.text =  sec.toString() + '.' + ms.toString();
// 		//console.log('clocktimer ', d.getSeconds(), d.getSeconds() - startTime, startTime)
// 	}, 100);
// }

function pad(val) {
	return val > 9 ? val : "0" + val;
}

function ActivateSubmitButton() {
	//revealBtn.visible = true;
	//revealBtn.addEventListener(MouseEvent.CLICK, RevealNetwork, false);
	parent.EndTrial();
}

function RevealNetwork() {
	//revealBtn.visible = false;
	//console.log('revealing true network');
	TurnOnEdges(network, edgesRevealed);

	counter.text = score + ' of ' + (N*(N-1))/2; //TODO+ Math.factorial(N-1, 2);
	timer.text = ' ';

	for (var i = 0; i < N; i++) {
		for (var j = 0; j < N; j++) {
			if (i > j) {
				edgeSpaces[i][j].visible = false;

				if (belief_network[i][j] == network[i][j] & belief_network[j][i] == network[j][i]) {
					console.log('correct', i,j);
					edgeSpacesRight[i][j].visible  = true;
				} else {
					console.log('wrong', i,j);
					edgeSpacesWrong[i][j].visible  = true;
				}
			}
		}
	}
}

function TurnOnEdges(network_inside, edges_inside) {
	//console.log('turn on edges', network, edges_inside)
	for (var i = 0; i < N; i++) {
		for (var j = 0; j < N; j++) {
			if (i != j) {
				if (network_inside[i][j] == 1) {
					edges_inside[i][j].visible = true;
					//console.log('drawing', i, j, edges_inside[i][j].x,
						//edges_inside[i][j].y, edges_inside[i][j])
					} else {
						edges_inside[i][j].visible = false;
					}
				}
			}
		}
	}

	function inside(point, vs) {
    // ray-casting algorithm based on
    // http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html

    var x = point[0], y = point[1];

    var inside = false;
    for (var i = 0, j = vs.length - 1; i < vs.length; j = i++) {
    	var xi = vs[i][0], yi = vs[i][1];
    	var xj = vs[j][0], yj = vs[j][1];

    	var intersect = ((yi > y) != (yj > y))
    	&& (x < (xj - xi) * (y - yi) / (yj - yi) + xi);
    	if (intersect) inside = !inside;
    }

    return inside;
};

function ScoreBonus()
{
	//var cur_bel = belief_trajectory[data.belief_trajectory.length][0];

	for (var i = 0; i < N; i++)
	{
		for (var j = 0; j < N; j++)
		{
			if (j<i)
			{
				if (network[i][j]==belief_network[i][j] & network[j][i]==belief_network[j][i])
				{
					bonusScore = bonusScore+1;
				}
			}
			
		}
	}

	console.log('bonus payment', belief_trajectory, belief_network, network, bonusScore);

}

function sign(x){
    if( +x === x ) { // check if a number was given
        return (x === 0) ? x : (x > 0) ? 1 : -1;
    }
    return NaN;
}