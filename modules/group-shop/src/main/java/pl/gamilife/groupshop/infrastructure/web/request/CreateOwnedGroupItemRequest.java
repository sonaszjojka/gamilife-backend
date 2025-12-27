package pl.gamilife.groupshop.infrastructure.web.request;

import jakarta.validation.constraints.NotNull;

import java.util.UUID;

public record CreateOwnedGroupItemRequest(

        @NotNull
        UUID groupItemId
) {
}
