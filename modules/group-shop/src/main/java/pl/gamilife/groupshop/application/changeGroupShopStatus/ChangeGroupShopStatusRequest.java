package pl.gamilife.groupshop.application.changeGroupShopStatus;

import jakarta.validation.constraints.NotNull;

public record ChangeGroupShopStatusRequest(
        @NotNull
        Boolean isActive
) {
}
