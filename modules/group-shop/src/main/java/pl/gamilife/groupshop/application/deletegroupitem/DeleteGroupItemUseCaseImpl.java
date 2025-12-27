package pl.gamilife.groupshop.application.deletegroupitem;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.exception.GroupShopItemNotFoundException;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.exception.InactiveGroupShopException;
import pl.gamilife.groupshop.domain.model.projection.GroupForShop;
import pl.gamilife.groupshop.domain.model.projection.GroupShopUser;
import pl.gamilife.groupshop.domain.port.context.CurrentUserContext;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.GroupItemRepository;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;
@AllArgsConstructor
@Service
public class DeleteGroupItemUseCaseImpl implements DeleteGroupItemUseCase {

    private final CurrentUserContext currentUserProvider;
    private final GroupContext groupProvider;
    private final GroupItemRepository groupItemRepository;
    private final GroupShopRepository groupShopRepository;

    @Transactional
    @Override
    public Void execute(DeleteGroupItemCommand cmd) {

        GroupShopUser currentUser = currentUserProvider.findGroupShopUserById(cmd.currentUserId());
        GroupForShop groupDto = groupProvider.findGroupById(cmd.groupId());

        GroupShop groupShop = groupShopRepository.findByGroupId(cmd.groupId()).orElseThrow(() ->
                new GroupShopNotFoundException("Group shop for the specified group not found!"));

        if (Boolean.FALSE.equals(groupShop.getIsActive())) {
            throw new InactiveGroupShopException("This group has group shop inactive!");
        }


        if (!currentUser.userId().equals(groupDto.adminId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can delete group item in shop!");
        }
        groupItemRepository.findById(cmd.groupItemId()).orElseThrow(
                () -> new GroupShopItemNotFoundException("Group item in shop with id: " + cmd.groupItemId() + " not found!"));

        groupItemRepository.deleteById(cmd.groupItemId());

        return null;
    }
}
