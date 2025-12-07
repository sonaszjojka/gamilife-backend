package pl.gamilife.groupshop.usecase.deleteownedgroupitem;

import org.springframework.stereotype.Service;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.group.GroupApi;
import pl.gamilife.api.group.dto.GroupDto;
import pl.gamilife.groupshop.entity.GroupShop;
import pl.gamilife.groupshop.exception.domain.GroupShopNotFoundException;
import pl.gamilife.groupshop.exception.domain.InactiveGroupShopException;
import pl.gamilife.groupshop.exception.domain.OwnedGroupItemNotFoundException;
import pl.gamilife.groupshop.repository.GroupShopRepository;
import pl.gamilife.groupshop.repository.OwnedGroupItemRpository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;

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
