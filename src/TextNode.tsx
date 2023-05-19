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
