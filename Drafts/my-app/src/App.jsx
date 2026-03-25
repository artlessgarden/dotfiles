import { useState } from "react";

function App() {
    const [keyword, setKeyword] = useState("");
    const [list, setList] = useState([]);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState("");

    const searchCharacters = async () => {
        if (!keyword.trim()) {
            setError("请输入角色名");
            setList([]);
            return;
        }

        try {
            setLoading(true);
            setError("");

            const res = await fetch("https://api.bgm.tv/v0/search/characters", {
                method: "POST",
                headers: {
                    "Content-Type": "application/json",
                },
                body: JSON.stringify({
                    keyword: keyword.trim(),
                }),
            });

            if (!res.ok) {
                throw new Error(`请求失败: ${res.status}`);
            }

            const data = await res.json();
            setList(data.data || []);
        } catch (err) {
            setError(err.message || "请求失败");
            setList([]);
        } finally {
            setLoading(false);
        }
    };

    return (
        <div style={styles.page}>
            <div style={styles.container}>
                <h1 style={styles.title}>Bangumi 角色搜索</h1>

                <div style={styles.searchBar}>
                    <input
                        style={styles.input}
                        value={keyword}
                        onChange={(e) => setKeyword(e.target.value)}
                        onKeyDown={(e) => {
                            if (e.key === "Enter") {
                                searchCharacters();
                            }
                        }}
                        placeholder="输入角色名"
                    />
                    <button style={styles.button} onClick={searchCharacters}>
                        搜索
                    </button>
                </div>

                {loading && <p>加载中...</p>}
                {error && <p style={styles.error}>{error}</p>}

                <div style={styles.grid}>
                    {list.map((item) => (
                        <div key={item.id} style={styles.card}>
                            <img
                                src={
                                    item.images?.medium ||
                                    item.images?.small ||
                                    ""
                                }
                                alt={item.name}
                                style={styles.image}
                            />
                            <div style={styles.cardBody}>
                                <h3 style={styles.name}>{item.name}</h3>
                                {item.name_cn ? (
                                    <p style={styles.subName}>{item.name_cn}</p>
                                ) : null}
                                <p style={styles.id}>ID: {item.id}</p>
                            </div>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
}

const styles = {
    page: {
        minHeight: "100vh",
        background: "#f5f5f5",
        padding: "40px 20px",
        boxSizing: "border-box",
    },
    container: {
        maxWidth: "1000px",
        margin: "0 auto",
    },
    title: {
        marginBottom: "20px",
    },
    searchBar: {
        display: "flex",
        gap: "12px",
        marginBottom: "24px",
    },
    input: {
        flex: 1,
        height: "42px",
        padding: "0 12px",
        fontSize: "16px",
        border: "1px solid #ccc",
        borderRadius: "8px",
        outline: "none",
    },
    button: {
        height: "42px",
        padding: "0 18px",
        border: "none",
        borderRadius: "8px",
        cursor: "pointer",
        fontSize: "16px",
    },
    error: {
        color: "red",
        marginBottom: "16px",
    },
    grid: {
        display: "grid",
        gridTemplateColumns: "repeat(auto-fill, minmax(180px, 1fr))",
        gap: "16px",
    },
    card: {
        background: "#fff",
        borderRadius: "12px",
        overflow: "hidden",
        boxShadow: "0 2px 8px rgba(0,0,0,0.08)",
    },
    image: {
        width: "100%",
        height: "240px",
        objectFit: "cover",
        background: "#eee",
    },
    cardBody: {
        padding: "12px",
    },
    name: {
        margin: "0 0 8px",
        fontSize: "16px",
    },
    subName: {
        margin: "0 0 8px",
        color: "#666",
        fontSize: "14px",
    },
    id: {
        margin: 0,
        color: "#999",
        fontSize: "12px",
    },
};

export default App;
