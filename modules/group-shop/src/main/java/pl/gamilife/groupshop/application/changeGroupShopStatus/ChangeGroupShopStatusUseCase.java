package pl.gamilife.groupshop.application.changeGroupShopStatus;

import java.util.UUID;

public interface ChangeGroupShopStatusUseCase {
    ChangeGroupShopStatusResponse execute(ChangeGroupShopStatusRequest request, UUID shopId, UUID groupId);
}
