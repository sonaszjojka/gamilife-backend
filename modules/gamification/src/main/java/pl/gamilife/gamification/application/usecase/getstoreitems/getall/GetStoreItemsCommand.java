package pl.gamilife.gamification.application.usecase.getstoreitems.getall;


import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.io.Serializable;
import java.util.List;


public record GetStoreItemsCommand(
        String itemName,
        List<Integer> itemSlot,
        List<Integer> rarity,
        @NotNull Integer page,
        @NotNull Integer size
) implements Command, Serializable {
}
