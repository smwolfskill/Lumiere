using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MusicManager : MonoBehaviour 
{
    public AudioClip[] music;
    private AudioSource source;
    private AudioClip lastPlayed;

	// Use this for initialization
	void Start () 
    {
        source = GetComponent<AudioSource> ();
        lastPlayed = null;
	}
	
	// Update is called once per frame
	void Update () 
    {
        if (source != null && (!source.isPlaying)) 
        {
            PlayRandomTrack ();
        }
	}

    void PlayRandomTrack()
    {
        AudioClip musicTrack = music [Random.Range (0, music.Length)];
        while (lastPlayed == musicTrack) 
        {
            musicTrack = music [Random.Range (0, music.Length)];
        }

        if (musicTrack != null) 
        {
            source.PlayOneShot (musicTrack);
        }
    }
}
