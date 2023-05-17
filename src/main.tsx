import React from 'react'
import ReactDOM from 'react-dom/client'
import './index.css'
import Flow from "./App.tsx";

ReactDOM.createRoot(document.getElementById('root') as HTMLElement).render(
    <React.StrictMode>
        <div style={{width: '100vw', height: '100vh'}}>
            <Flow/>
        </div>
    </React.StrictMode>,
)
