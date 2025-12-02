package edu.pjwstk.groupshop.usecase.changeGroupShopStatus;

import java.util.UUID;

public interface ChangeGroupShopStatusUseCase {
    ChangeGroupShopStatusResponse execute(ChangeGroupShopStatusRequest request, UUID shopId, UUID groupId);
}
