async function searchCharacters(keyword) {
    try {
        const res = await fetch("https://api.bgm.tv/v0/search/characters", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({
                keyword: keyword,
                filter: {
                    nsfw: false,
                },
            }),
        });

        if (!res.ok) {
            throw new Error("请求失败");
        }

        const data = await res.json();

        console.log(data);

        return data.data; // 只返回角色数组
    } catch (err) {
        console.error("出错:", err);
        return [];
    }
}
