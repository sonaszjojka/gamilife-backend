package edu.pjwstk.groupshop.usecase.deleteownedgroupitem;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.groups.GroupApi;
import edu.pjwstk.api.groups.dto.GroupDto;
import edu.pjwstk.core.exception.common.GroupAdminPrivilegesRequiredException;
import edu.pjwstk.groupshop.entity.GroupShop;
import edu.pjwstk.groupshop.exception.GroupShopNotFoundException;
import edu.pjwstk.groupshop.exception.InactiveGroupShopException;
import edu.pjwstk.groupshop.exception.OwnedGroupItemNotFoundException;
import edu.pjwstk.groupshop.repository.GroupShopRepository;
import edu.pjwstk.groupshop.repository.OwnedGroupItemRpository;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class DeleteOwnedGroupItemUseCaseImpl implements DeleteOwnedGroupItemUseCase {
    private final OwnedGroupItemRpository ownedGroupItemRpository;
    private final AuthApi currentUserProvider;
    private final GroupApi groupApi;
    private final GroupShopRepository groupShopRepository;

    public DeleteOwnedGroupItemUseCaseImpl(OwnedGroupItemRpository ownedGroupItemRpository, AuthApi currentUserProvider, GroupApi groupApi, GroupShopRepository groupShopRepository) {
        this.ownedGroupItemRpository = ownedGroupItemRpository;
        this.currentUserProvider = currentUserProvider;
        this.groupApi = groupApi;
        this.groupShopRepository = groupShopRepository;
    }

    @Override
    public void execute(UUID groupId, UUID memberId, UUID ownedGroupItemId) {

        ownedGroupItemRpository.findById(ownedGroupItemId).orElseThrow(
                () -> new OwnedGroupItemNotFoundException("Owned group item not found"));


        GroupShop groupShop = groupShopRepository.findByGroupId(groupId).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        CurrentUserDto currentUser = currentUserProvider.getCurrentUser();
        GroupDto groupDto = groupApi.findGroupById(groupId);
        if (!currentUser.userId().equals(groupDto.adminId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can delete items from inventory!");
        }
        ownedGroupItemRpository.deleteById(ownedGroupItemId);

    }
}
