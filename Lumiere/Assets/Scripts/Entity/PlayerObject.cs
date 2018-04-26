using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Runtime.CompilerServices;
using UnityEngine.SceneManagement;

public class PlayerObject : EntityObject
{
    private GameObject healthBar;
    private Animator anim;

    public EquipmentManager EquipmentManager
    {
        get;
        protected set;
    }

    public PlayerObject(GameObject existingGameObject, float maxHealth) : base(existingGameObject, maxHealth)
    {
        this.inventory = new Inventory(5, 3);
        this.EquipmentManager = new EquipmentManager(10, 1);
        this.healthBar = GameObject.Find("PanelHealthBarFill");
        GameObject UICanvas = GameObject.FindGameObjectWithTag("UICanvas");
        UIBehavior uib = UICanvas.GetComponent<UIBehavior>();

        Camera.main.GetComponent<CameraFollow>().SetTargetTransform(existingGameObject.transform);
        GameObject inventoryPanel = uib.inventoryPanel;
        inventoryPanel.GetComponent<InventoryPanel>().SetInitialInventory(this.inventory);

        GameObject equipmentPanel = GameObject.FindGameObjectWithTag("EquipmentPanel");
        equipmentPanel.GetComponent<EquipmentPanel>().Manager = this.EquipmentManager;

        GameObject hotbarPanel = GameObject.FindGameObjectWithTag("HotbarPanel");
        hotbarPanel.GetComponent<HotbarPanel>().SetEquipmentManager(this.EquipmentManager);

        if (this.gameObject != null)
        {
            anim = this.gameObject.GetComponent<Animator>();
            //Set mass to 0 to prevent player from pushing around monsters
            Rigidbody2D playerRigidbody = this.gameObject.GetComponent<Rigidbody2D>();
            if (playerRigidbody != null)
            {
                playerRigidbody.mass = 0.0f;

            }
        }
    }

    public override void InflictDamage(float damageAmount)
    {
        float armor = ((float)EquipmentManager.GetArmorRating());
        float multiplier = (float)(1 - (0.05 * armor / (1 + 0.05 * Mathf.Abs(armor))));
        base.InflictDamage(damageAmount * multiplier);
        //Debug.Log ("Inflicting damage! Health: " + currHealth + " Max Health: " + maxHealth);
        UpdateHealthBar();
    }

    public override void Heal(float healAmount)
    {
        base.Heal(healAmount);
        UpdateHealthBar();
    }

    private void UpdateHealthBar()
    {
        this.healthBar.GetComponent<HealthBarManager>().SetHealth(currHealth / maxHealth);
    }

    protected override void Die()
    {
        anim.SetTrigger("TDie");
        PlaySound();
        GameObject.Destroy(gameObject, 1f);
        this.isDead = true;
        //TODO: Show Game Over screen
        /*UnityEngine.SceneManagement.Scene scene = UnityEngine.SceneManagement.SceneManager.GetActiveScene(); //current scene
        UnityEngine.SceneManagement.SceneManager.LoadSceneAsync(scene.name, LoadSceneMode.Single);*/
        GameObject uiCanvas = GameObject.FindGameObjectWithTag("UICanvas");
        if (uiCanvas != null)
        {
            UIBehavior uiBehavior = uiCanvas.GetComponent<UIBehavior>();
            uiBehavior.GameOver();
        }
    }
}
