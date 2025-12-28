package pl.gamilife.gamification.application.usecase.getuserinventoryitems;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.PositiveOrZero;
import pl.gamilife.shared.kernel.architecture.Command;

import java.io.Serializable;
import java.util.UUID;

public record GetUserInventoryItemsCommand(
        @NotNull
        UUID userId,

        String itemName,

        Integer itemSlot,

        Integer rarity,

        @PositiveOrZero
        Integer page,

        @Positive
        Integer size
) implements Command, Serializable {
}