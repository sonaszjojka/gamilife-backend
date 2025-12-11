package pl.gamilife.gamification.application.usecase.getstoreitems.getall;

import pl.gamilife.shared.kernel.architecture.Command;

import java.io.Serializable;
import java.util.List;


public record GetStoreItemsCommand(
        String itemName,
        List<Integer> itemSlot,
        List<Integer> rarity,
        Integer page,
        Integer size)implements Command, Serializable {
    @Override
    public void validate() {

    }
}
