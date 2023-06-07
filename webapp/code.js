/*
const evtSource = new EventSource("/watch");

evtSource.addEventListener("fileStoreChanges", (event) => {
  console.log("New event: " + event);
  const newElement = document.createElement("li");
  const eventList = document.getElementById("list");

  newElement.textContent = `message: ${event.data}`;
  eventList.appendChild(newElement);
});

function MyApp() {
  return <h1>Hello, world!</h1>;
}
const container = document.getElementById('root');
const root = ReactDOM.createRoot(container);
root.render(<MyApp />);
*/

const renderContent = (data, handler) => {
  if (typeof data === 'string' || data instanceof String) {
    return (<p>{data}</p>)
  } else {
    const elems = []
    for (let i = 0; i < data.length; i++) {
      const item = data[i]
      const docId = item.ml_docId
      if (docId) {
        elems.push(<li key={i}><a className="linkHand" onClick={() => handler(docId)}>{item.ml_text}</a></li>)
      } else {
        elems.push(<li key={i}>{item.ml_text}</li>)
      }
    }
    return (<ul>{elems}</ul>)
  }
}

const renderSection = (data, handler) => {
  const title = data.ms_title
  const elems = []
  if (title) {
    elems.push(<h4 key="title">{title}</h4>)
  }
  data.ms_content.forEach((c, idx) => {
    elems.push(<div key={idx}>{renderContent(c, handler)}</div>)
  });
  return elems
}

const renderMDoc = (data, handler) => {
  const title = data.m_title
  const elems = []
  if (title) {
    elems.push(<h3 key="title">{title}</h3>)
  }
  data.m_sections.forEach((sect, idx) => {
    elems.push(<div key={idx}>{renderSection(sect, handler)}</div>)
  })
  return elems
}

const MDoc = ({docId, version, openDoc}) => {
  const [doc, setDoc] = React.useState(null)

  React.useEffect(() => {
    const url = `/doc/${docId}/${version}`
    fetch(url)
      .then((response) => {
        if (!response.ok) {
          throw new Error(`HTTP error for {url}: ${response.status}`)
        }
        return response.json();
      })
      .then((actualData) => {
        setDoc(actualData)
      })
      .catch((err) => {
        console.log(err.message);
      })
    }, [docId, version]);

  if (doc === null) {
    return <p>Loading {docId}/{version} ...</p>
  } else {
    return renderMDoc(doc, openDoc)
  }

}

const HospitalApp = () => {
  // An array with elements of the form {"docId": docId, "version": version}
  const [docs, setDocs] = React.useState([{"docId": "index", "version": 0}])

  React.useEffect(() => {
    // NOTE: this code is executed only once, at mount of the component.
    let eventSource = new EventSource("/watch")
    eventSource.addEventListener("fileStoreChanges", (event) => {
      const docIdVersionDict = JSON.parse(event.data)
      // docIdVersionDict is a dictionary mapping docIds to versions
      setDocs((docs) => {
        console.log(`Change: ${event.data}, docs: ${JSON.stringify(docs)}`)
        let changes = false
        const newDocs = docs.map((d) => {
          const v = docIdVersionDict[d.docId]
          if (v && v !== d.version) {
            changes = true
            return {"docId": d.docId, "version": v}
          } else {
            return d
          }
        })
        if (changes) {
          console.log(`Docs: ${JSON.stringify(docs)} ==> ${JSON.stringify(newDocs)}`)
          return newDocs
        } else {
          return docs
        }
      })
    })
    return () => eventSource.close()
  }, [])

  const openDoc = (docId) => {
    setDocs((docs) => {
      if (docs.length === 2 && docs[1].docId === docId) {
        console.log(`Document ${docId} is already open`)
        return docs
      }
      console.log(`Opening document ${docId}`)
      const newDocs = [docs[0], {"docId": docId, "version": 0}]
      console.log(`Docs: ${JSON.stringify(docs)} ==> ${JSON.stringify(newDocs)}`)
      return newDocs
    })
  }

  const divs = docs.map(d =>
    <div key={d.docId} className="col">
      <MDoc docId={d.docId} version={d.version} openDoc={openDoc}/>
    </div>
  );
  return (
    <>{divs}</>
  )
}

const container = document.getElementById('root');
const root = ReactDOM.createRoot(container);
root.render(<HospitalApp />);
