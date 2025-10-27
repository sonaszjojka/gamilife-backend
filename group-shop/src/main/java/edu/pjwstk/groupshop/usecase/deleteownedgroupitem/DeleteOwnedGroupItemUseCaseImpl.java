package edu.pjwstk.groupshop.usecase.deleteownedgroupitem;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.GroupApi;
import edu.pjwstk.common.groupsApi.dto.GroupDto;
import edu.pjwstk.groupshop.exception.OwnedGroupItemNotFoundException;
import edu.pjwstk.groupshop.exception.UserNotAdministratorException;
import edu.pjwstk.groupshop.repository.OwnedGroupItemRpository;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class DeleteOwnedGroupItemUseCaseImpl implements DeleteOwnedGroupItemUseCase {
private final OwnedGroupItemRpository ownedGroupItemRpository;
private final AuthApi currentUserProvider;
private final GroupApi groupApi;

    public DeleteOwnedGroupItemUseCaseImpl(OwnedGroupItemRpository ownedGroupItemRpository, AuthApi currentUserProvider, GroupApi groupApi) {
        this.ownedGroupItemRpository = ownedGroupItemRpository;
        this.currentUserProvider = currentUserProvider;
        this.groupApi = groupApi;
    }

    @Override
    public void execute(UUID groupId, UUID memberId, UUID ownedGroupItemId) {

        ownedGroupItemRpository.findById(ownedGroupItemId).orElseThrow(
                ()->new OwnedGroupItemNotFoundException("Owned group item not found"));

        CurrentUserDto currentUser = currentUserProvider.getCurrentUser().orElseThrow();
        GroupDto groupDto = groupApi.findGroupById(groupId);
        if (!currentUser.userId().equals(groupDto.adminId())) {
            throw new UserNotAdministratorException("Only group administrators can delete items from inventory!");
        }
        ownedGroupItemRpository.deleteById(ownedGroupItemId);

    }
}
