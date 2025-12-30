package pl.gamilife.groupshop.application.getgroupshopitems;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.model.filter.GroupItemsFilter;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.GroupItemRepository;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetGroupShopItemsUseCaseImpl implements GetGroupShopItemsUseCase {

    private final GroupContext groupContext;
    private final GroupShopRepository groupShopRepository;
    private final GroupItemRepository groupItemRepository;

    @Override
    public Page<GetGroupShopItemsResult> execute(GetGroupShopItemsCommand cmd) {
        var group = groupContext.findGroupById(cmd.groupId());

        if (!group.adminId().equals(cmd.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException("User is not an admin of the group");
        }

        var groupShop = groupShopRepository.findByGroupId(cmd.groupId()).orElseThrow(
                () -> new GroupShopNotFoundException("Group shop not found")
        );

        var groupItemsPage = groupItemRepository.findAll(
                new GroupItemsFilter(groupShop.getId(), cmd.isActive()),
                cmd.page(),
                cmd.size()
        );

        return groupItemsPage.map(gi -> new GetGroupShopItemsResult(
                gi.getId(),
                gi.getName(),
                gi.getPrice(),
                gi.getIsActive()
        ));
    }
}
