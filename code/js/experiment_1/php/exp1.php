<?php

// Create a database connection


	$connection = mysql_connect("mysql-server.ucl.ac.uk","ucjtw3g","vXq37JTQc4J22R9uIITd");//1Ammooi9


	
	if (!$connection)

	{

		die("Database connection failed: " . mysql_error());

	}

	// Select a database to use
	$db_select = mysql_select_db("ucjtw3g", $connection);

	
	if (!$db_select)

	{

		die("Database connection failed: " . mysql_error());

	}

	
	// Get values passed from Flash
	$ip = $_SERVER['REMOTE_ADDR'];
	$date = date('Y-m-d');
	$events = $_POST['events'];
	$beliefs = $_POST['beliefs'];
	$trials = $_POST['trials'];
	$extras = $_POST['extras'];
	$feedback = $_POST['feedback'];

	$query = "INSERT INTO neil_time2 (ip, date, events, beliefs, trials, extras, feedback) VALUES ('{$ip}', '{$date}', '{$events}', '{$beliefs}', '{$trials}', '{$extras}', '{$feedback}')";
	mysql_query($query, $connection);


	// Close connection
	mysql_close($connection);

?>