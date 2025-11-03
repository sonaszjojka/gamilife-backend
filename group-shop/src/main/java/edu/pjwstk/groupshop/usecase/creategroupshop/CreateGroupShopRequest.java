package edu.pjwstk.groupshop.usecase.creategroupshop;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

public record CreateGroupShopRequest(
        @NotNull
        @Size(max =100)
        String name,

        @NotNull
        @Size(max=300)
        String description
) {
}
