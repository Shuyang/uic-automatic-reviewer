package edu.uic.cs.automatic_reviewer.input.parse;

import com.itextpdf.text.pdf.parser.ImageRenderInfo;
import com.itextpdf.text.pdf.parser.RenderListener;
import com.itextpdf.text.pdf.parser.TextRenderInfo;

class ImageCounter implements RenderListener
{
	private int count = 0;

	@Override
	public void beginTextBlock()
	{
	}

	@Override
	public void renderText(TextRenderInfo renderInfo)
	{
	}

	@Override
	public void endTextBlock()
	{
	}

	@Override
	public void renderImage(ImageRenderInfo renderInfo)
	{
		count++;
	}

	public int getCount()
	{
		return count;
	}

}
