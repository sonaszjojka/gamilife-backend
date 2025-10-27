package edu.pjwstk.groupshop.usecase.editownedgroupitem;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.GroupApi;
import edu.pjwstk.common.groupsApi.dto.GroupDto;
import edu.pjwstk.common.groupsApi.exception.GroupMemberNotFoundException;
import edu.pjwstk.groupshop.entity.OwnedGroupItem;
import edu.pjwstk.groupshop.exception.UnauthorizedUserActionException;
import edu.pjwstk.groupshop.exception.UserNotAdministratorException;
import edu.pjwstk.groupshop.repository.OwnedGroupItemRpository;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.UUID;

@Service
public class EditOwnedGroupItemUseCaseImpl implements EditOwnedGroupItemUseCase {

    private final OwnedGroupItemRpository ownedGroupItemRpository;
    private final AuthApi currentUserProvider;
    private final GroupApi groupProvider;
    private final EditOwnedGroupItemMapper editOwnedGroupItemMapper;

    public EditOwnedGroupItemUseCaseImpl(OwnedGroupItemRpository ownedGroupItemRpository, AuthApi currentUserProvider, GroupApi groupProvider, EditOwnedGroupItemMapper editOwnedGroupItemMapper) {
        this.ownedGroupItemRpository = ownedGroupItemRpository;
        this.currentUserProvider = currentUserProvider;
        this.groupProvider = groupProvider;
        this.editOwnedGroupItemMapper = editOwnedGroupItemMapper;
    }

    @Override
    public EditOwnedGroupItemResponse execute(EditOwnedGroupItemRequest request, UUID ownedGroupItemId, UUID groupMemberId, UUID groupId) {

        if (groupProvider.findGroupMemberById(groupMemberId)==null) {
            throw new GroupMemberNotFoundException("Group member not found in the specified group!");
        }

        CurrentUserDto currentUserDto = currentUserProvider.getCurrentUser().orElseThrow();
        GroupDto groupDto = groupProvider.findGroupById(groupId);
        if (!currentUserDto.userId().equals(groupDto.adminId()) && !currentUserDto.userId().equals(groupProvider.findGroupMemberById(groupMemberId).userId())) {
            throw new UnauthorizedUserActionException("Only group administrators or the member themselves can edit items in inventory!");
        }

        OwnedGroupItem ownedGroupItem = ownedGroupItemRpository.findById(ownedGroupItemId).orElseThrow(
                () -> new RuntimeException("Owned group item not found")
        );

        if (!currentUserDto.userId().equals(groupDto.adminId()) &&
                Boolean.TRUE.equals(ownedGroupItem.getIsUsedUp())&&
                Boolean.FALSE.equals(request.isUsedUp())) {
            throw new UserNotAdministratorException( "Only group administrators can mark used up items as unused!" );

        }
        ownedGroupItem.setIsUsedUp(request.isUsedUp());
        if (Boolean.TRUE.equals(request.isUsedUp())) {
            ownedGroupItem.setUseDate(Instant.now());
        }

        if (currentUserDto.userId().equals(groupDto.adminId())&&
                Boolean.TRUE.equals(ownedGroupItem.getIsUsedUp())&&
                Boolean.FALSE.equals(request.isUsedUp()))
        {
            ownedGroupItem.setUseDate(null);
        }

        return editOwnedGroupItemMapper.toResponse(ownedGroupItemRpository.save(ownedGroupItem));
    }
}
