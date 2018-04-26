using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Room/Container")]
public class ContainerType : BaseObject
{
    public GameObject PopulateGameObject()
    {
        return new GameObject(name);
    }

}
