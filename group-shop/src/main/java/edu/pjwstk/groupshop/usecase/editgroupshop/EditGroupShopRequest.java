package edu.pjwstk.groupshop.usecase.editgroupshop;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

public record EditGroupShopRequest(
        @NotNull
        @Size(max = 100)
        String name,

        @NotNull
        @Size(max = 300)
        String description
) {
}
