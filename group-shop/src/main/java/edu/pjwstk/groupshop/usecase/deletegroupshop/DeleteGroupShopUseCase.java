package edu.pjwstk.groupshop.usecase.deletegroupshop;


import java.util.UUID;

public interface DeleteGroupShopUseCase {

    void execute(UUID groupShopId, UUID groupId);
}
