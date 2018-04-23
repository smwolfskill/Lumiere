using UnityEngine;
//using UnityEditor;
using UnityEngine.TestTools;
using NUnit.Framework;
using System.Collections;
//using UnityEditor.VersionControl;
using NUnit.Framework.Internal;

public class EntitySpriteManagerTest 
{
	public EntitySpriteManager spriteMgr;
	public Entity entity;
	public GameObject testObject;

	/// <summary>
	/// Run only once before tests run. Load GamePlayer entity and set up EntitySpriteManager.
	/// </summary>
	[SetUp]
	public void Init() 
	{
		entity = Resources.Load<Entity>("GamePlayer");
		testObject = new GameObject("testObject", typeof(SpriteRenderer), typeof(EntitySpriteManager));
		spriteMgr = testObject.GetComponent<EntitySpriteManager>();
		spriteMgr.entity = entity;
		Assert.IsNotNull(entity);
		Assert.IsNotNull(testObject.GetComponent<SpriteRenderer>());
	}

	/// <summary>
	/// Called after all tests finish. Free GamePlayer asset.
	/// </summary>
	[OneTimeTearDown]
	public void Cleanup() 
	{
		if(entity != null) 
		{
			Resources.UnloadAsset(entity);
		}
	}

	/// <summary>
	/// Test if EntitySpriteManager correctly sets its GameObject's sprite.
	/// </summary>
	[Test]
	public void SetSpriteTest() 
	{
		spriteMgr.SetSprite();
		SpriteRenderer renderer = testObject.GetComponent<SpriteRenderer>();
		Sprite setSprite = renderer.sprite;
		Assert.AreEqual(entity.GetSprite(), setSprite);
	}

}
