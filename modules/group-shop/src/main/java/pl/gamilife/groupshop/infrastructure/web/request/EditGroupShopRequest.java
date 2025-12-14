package pl.gamilife.groupshop.infrastructure.web.request;

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
