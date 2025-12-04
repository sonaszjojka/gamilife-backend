package pl.gamilife.groupshop.usecase.changeGroupShopStatus;

import jakarta.validation.constraints.NotNull;

public record ChangeGroupShopStatusRequest(
        @NotNull
        Boolean isActive
) {
}
