using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;

[CreateAssetMenu(menuName = "Lumiere/Utilities/SpriteListCollection")]
public class SpriteListCollection : ScriptableObject
{
    public SpriteListDict[] namedSpriteLists;

    [Serializable]
    public struct SpriteListDict
    {
        public string key;
        public SpriteList value;
    }

    public SpriteList GetSpriteList(string key)
    {
        foreach (SpriteListDict spriteListDict in namedSpriteLists)
        {
            if (spriteListDict.key == key)
            {
                return spriteListDict.value;
            }
        }
        return null;
    }
}
