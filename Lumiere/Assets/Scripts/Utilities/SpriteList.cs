using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;

[CreateAssetMenu(menuName = "Lumiere/Utilities/SpriteList")]
public class SpriteList : ScriptableObject
{
    public SpriteDict[] namedSprites;

    [Serializable]
    public struct SpriteDict
    {
        public string key;
        public Sprite value;
    }

    public Sprite GetSprite(string key)
    {
        foreach (SpriteDict spriteDict in namedSprites)
        {
            if (spriteDict.key == key)
            {
                return spriteDict.value;
            }
        }
        return null;
    }
}
