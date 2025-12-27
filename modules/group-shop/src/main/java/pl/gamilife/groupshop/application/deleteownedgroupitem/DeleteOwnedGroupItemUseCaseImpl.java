package pl.gamilife.groupshop.application.deleteownedgroupitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.exception.InactiveGroupShopException;
import pl.gamilife.groupshop.domain.exception.OwnedGroupItemNotFoundException;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.model.projection.GroupForShop;
import pl.gamilife.groupshop.domain.model.projection.GroupShopUser;
import pl.gamilife.groupshop.domain.port.context.CurrentUserContext;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.groupshop.domain.port.repository.OwnedGroupItemRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;

@Service
@AllArgsConstructor
public class DeleteOwnedGroupItemUseCaseImpl implements DeleteOwnedGroupItemUseCase {
    private final OwnedGroupItemRepository ownedGroupItemRepository;
    private final CurrentUserContext currentUserProvider;
    private final GroupContext groupApi;
    private final GroupShopRepository groupShopRepository;

    @Transactional
    @Override
    public Void execute(DeleteOwnedGroupItemCommand cmd) {

        ownedGroupItemRepository.findById(cmd.ownedGroupItemId()).orElseThrow(
                () -> new OwnedGroupItemNotFoundException("Owned group item not found"));


        GroupShop groupShop = groupShopRepository.findByGroupId(cmd.groupId()).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }

        GroupShopUser currentUser = currentUserProvider.findGroupShopUserById(cmd.currentUserId());
        GroupForShop groupDto = groupApi.findGroupById(cmd.groupId());
        if (!currentUser.userId().equals(groupDto.adminId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can delete items from inventory!");
        }
        ownedGroupItemRepository.deleteById(cmd.ownedGroupItemId());

        return null;
    }
}
