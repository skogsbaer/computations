const evtSource = new EventSource("/watch");

evtSource.addEventListener("fileStoreChanges", (event) => {
  console.log("New event: " + event);
  const newElement = document.createElement("li");
  const eventList = document.getElementById("list");

  newElement.textContent = `message: ${event.data}`;
  eventList.appendChild(newElement);
});
