using UnityEngine;
using System.Collections;


public class AttackAnimation : MonoBehaviour
{

    public void CreateAnimation(Player player, GameObject animationObject, WeaponItem weapon, Transform targetTransform, float animationSpeed)
    {
        //Set the animation's sprite
        //TODO make or find a sprite for fisticuffs attacks
        Sprite weaponSprite = weapon == null ? player.sprite : weapon.GroundSprite;
        SpriteRenderer animationSpriteRenderer = animationObject.GetComponent<SpriteRenderer>();
        animationSpriteRenderer.sprite = weaponSprite;

        //Calculate relative location of the enemy
        Vector3 targetLoc = targetTransform.position - animationObject.transform.position;

        Animation attackAnimation = animationObject.GetComponent<Animation>();

        AnimationClip attackClip = new AnimationClip();
        attackClip.legacy = true;

        AnimationEvent showSprite = new AnimationEvent();
        showSprite.functionName = "EnableSprite";
        showSprite.objectReferenceParameter = animationSpriteRenderer;
        showSprite.time = 0.0f;

        AnimationEvent hideSprite = new AnimationEvent();

        hideSprite.functionName = "DisableSprite";
        hideSprite.objectReferenceParameter = animationSpriteRenderer;
        hideSprite.time = 2.0f;

        attackClip.AddEvent(showSprite);
        attackClip.AddEvent(hideSprite);

        // create curves to move the sprite
        //Create shared initial and end frames
        Keyframe initFrame = new Keyframe(0.0f, 0.0f);
        Keyframe endFrame = new Keyframe(2.0f, 0.0f);

        //Create keyframe array for x position transformation
        Keyframe[] xKeys = new Keyframe[] { initFrame, new Keyframe(1.0f, targetLoc.x), endFrame };
        AnimationCurve xCurve = new AnimationCurve(xKeys);

        //Create keyframe array for y position transformation
        Keyframe[] yKeys = new Keyframe[] { initFrame, new Keyframe(1.0f, targetLoc.y), endFrame };
        AnimationCurve yCurve = new AnimationCurve(yKeys);



        attackClip.SetCurve("", typeof(Transform), "localPosition.x", xCurve);
        attackClip.SetCurve("", typeof(Transform), "localPosition.y", yCurve);

        attackAnimation.AddClip(attackClip, "attack");

        attackAnimation["attack"].speed = animationSpeed;

        Debug.Log("animation speed: " + animationSpeed.ToString());

        //attackAnimation.Play("attack");
        Vector3 endLoc = animationObject.transform.position;
    }

    public void EnableSprite(SpriteRenderer animationSpriteRenderer)
    {
        animationSpriteRenderer.enabled = true;
        Debug.Log("Enabling sprite renderer");
    }

    public void DisableSprite(SpriteRenderer animationSpriteRenderer)
    {
        animationSpriteRenderer.enabled = false;
        Debug.Log("Disabling sprite renderer");
    }
}