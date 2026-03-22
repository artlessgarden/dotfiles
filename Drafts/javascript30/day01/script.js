window.addEventListener("keydown", (e) => {
    console.log(`tap the ${e.key}`);
    const elkey = document.querySelector(`div[data-key="${e.key}"]`);
    const elaudio = document.querySelector(`audio[data-key="${e.key}"]`);
    if (!elkey) console.log("not a difine key");
    else {
        console.log(elkey);
        console.log(elaudio);
        elkey.classList.add("playing");
        elaudio.currentTime = 0;
        elaudio.play();
    }
});
window.addEventListener("keyup", (e) => {
    const elkey = document.querySelector(`div[data-key="${e.key}"]`);
    if (!elkey);
    else elkey.classList.remove("playing");
});
