package edu.pjwstk.groupshop.usecase.creategroupiteminshop;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.PositiveOrZero;
import jakarta.validation.constraints.Size;

public record CreateGroupItemInShopRequest(
        @NotNull
        @Size(min = 1, max = 30)
        String name,

        @NotNull
        @PositiveOrZero
        Integer price,

        @NotNull
        Boolean isActive


) {
}
