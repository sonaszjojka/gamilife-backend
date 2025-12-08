package pl.gamilife.gamification.application.usecase.getstoreitems.getall;

import pl.gamilife.shared.kernel.architecture.Command;

import java.io.Serializable;


public record GetStoreItemsCommand(
        String itemName,
        Integer itemSlot,
        Integer rarity,
        Integer page,
        Integer size)implements Command, Serializable {
    @Override
    public void validate() {

    }
}
