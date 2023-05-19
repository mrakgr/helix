import { Handle, Position } from 'reactflow';

interface TextNodeData {
    data: {
        content: string;
        onContentChange: (x: string) => void;
    };
}
export function TextNode({ data }: TextNodeData) {
    // We'll add more target handles later.
    return (
        <div className="grid w-96 h-96 p-8 card bg-base-300 rounded-box place-items-center">
            <textarea className="textarea w-full h-full textarea-bordered resize-none nodrag"
                placeholder="Text"
                value={data.content}
                onChange={x => { data.onContentChange(x.target.value); }}
            ></textarea>
            <Handle type="target"
                position={Position.Top}
                className="w-8 h-8 rounded-none bg-teal-500"
            />
            <Handle type="source"
                position={Position.Bottom}
                className="w-8 h-8 rounded-none bg-teal-500"
            />
        </div>
    );
}

interface CompilationNodeOutputData {
    data: {
        result: JSX.Element
    }
}

export function CompilationNode() {
    return (
        <div className="grid w-fit h-fit p-8 card bg-base-300 rounded-box place-items-center">
            <button className="btn btn-lg">Compile</button>
            <Handle type="target"
                position={Position.Top}
                className="w-8 h-8 rounded-none bg-red-500"
            />
        </div>
    );
}

export function CompilationOutputNode() {
    const ar : JSX.Element[] = []
    for (let i = 0; i < 30; i++) {
        ar.push(<p key={i}>If a dog chews shoes whose shoes does he choose?</p>)
    }
    return (
        <div className="card w-fit h-fit bg-base-100 border-8">
            <div className="card-body">
                <div className="flex flex-row justify-between">
                    <h2 className="card-title">Compilation Output</h2>
                    <div className="card-actions">
                        <button className="btn btn-primary">Close</button>
                    </div>
                </div>
                {ar}
            </div>
        </div>
    );
}