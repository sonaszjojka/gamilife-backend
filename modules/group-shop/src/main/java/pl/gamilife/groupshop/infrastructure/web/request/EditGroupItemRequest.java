package pl.gamilife.groupshop.infrastructure.web.request;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.PositiveOrZero;
import jakarta.validation.constraints.Size;

public record EditGroupItemRequest(


        @Size(min = 1, max = 30)
        String name,

        @PositiveOrZero
        Integer price,

        @NotNull
        Boolean isActive
) {
}
