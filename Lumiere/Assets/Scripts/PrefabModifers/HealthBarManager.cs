using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class HealthBarManager : MonoBehaviour {

    RectTransform rt;
    RectTransform parentRt;

    public float percent = 1.0f;

	void Start ()
    {
        rt = GetComponent<RectTransform>();
        parentRt = transform.parent.GetComponent<RectTransform>();
    }

    private void Update()
    {
    }

    public void SetHealth(float percent)
    {
        percent -= 1.0f;

        float right = parentRt.rect.width * (percent);
        rt.offsetMax = new Vector2(right, rt.offsetMax.y);
    }

}
