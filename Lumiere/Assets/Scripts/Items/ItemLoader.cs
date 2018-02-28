using UnityEngine;
using UnityEditor;
using System.IO;

/// <summary>
/// A static class that loads specified item data from a file and returns that data in the form of an item.
/// </summary>
public static class ItemLoader
{
    /// <summary>
    /// Loads an item by filename into the form of a GameItem object.
    /// </summary>
    /// <param name="filename">Location where information is stored.</param>
    /// <returns>A functional GameItem with the data from the file.</returns>
    public static GameItem LoadItem(string filename)
    {
        if (File.Exists(filename))
        {
            string fileData = File.ReadAllText (filename);
            GameItem loadedItem = JsonUtility.FromJson<GameItem> (fileData);
            return loadedItem;
        }
        return null;
    }

    /// <summary>
    /// Convert a GameItem into a pretty print JSON string and write it to a file, overwriting any existing contents.
    /// </summary>
    /// <returns><c>true</c>, if item was saved, <c>false</c> otherwise.</returns>
    /// <param name="item">Item.</param>
    /// <param name="filename">Filename.</param>
    public static bool SaveItem(GameItem item, string filename)
    {
        string serializedItem = JsonUtility.ToJson(item, true);
        try
        {
            File.WriteAllText (filename, serializedItem);
            return true;
        }
        catch
        {
            return false;
        }
    }
}