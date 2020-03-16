var count = 0;

/* exported clicked */
function clicked() {
	var counter = document.getElementById("counter");
	count++;
	counter.innerText = count;
}
