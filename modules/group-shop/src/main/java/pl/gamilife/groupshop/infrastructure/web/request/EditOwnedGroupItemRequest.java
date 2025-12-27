package pl.gamilife.groupshop.infrastructure.web.request;

import jakarta.validation.constraints.NotNull;

public record EditOwnedGroupItemRequest(

        @NotNull
        Boolean isUsedUp
) {
}
