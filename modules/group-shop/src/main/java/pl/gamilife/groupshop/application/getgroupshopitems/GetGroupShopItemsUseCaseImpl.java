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
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetGroupShopItemsUseCaseImpl implements GetGroupShopItemsUseCase {

    private final GroupShopRepository groupShopRepository;
    private final GroupItemRepository groupItemRepository;
    private final GroupContext groupContext;

    @Override
    public Page<GetGroupShopItemsResult> execute(GetGroupShopItemsCommand cmd) {
        var member = groupContext.findMemberByUserId(cmd.userId(), cmd.groupId()).orElseThrow(
                () -> new GroupMemberNotFoundException("User is not a member of the group")
        );

        if (!member.isAdmin()) {
            throw new GroupAdminPrivilegesRequiredException("User is not an admin of the group");
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
