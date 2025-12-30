package pl.gamilife.groupshop.application.getgroupshopdetails;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.GroupItemRepository;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetGroupShopDetailsUseCaseImpl implements GetGroupShopDetailsUseCase {

    private final GroupShopRepository groupShopRepository;
    private final GroupItemRepository groupItemRepository;
    private final GroupContext groupContext;

    @Override
    public GetGroupShopDetailsResult execute(GetGroupShopDetailsCommand cmd) {
        groupContext.findMemberByUserId(cmd.userId(), cmd.groupId()).orElseThrow(
                () -> new ResourceOwnerPrivilegesRequiredException("User is not a member of the group")
        );

        var groupShop = groupShopRepository.findByGroupId(cmd.groupId())
                .orElseThrow(() -> new GroupShopNotFoundException("Group shop not found"));

        Page<GroupItem> groupItemsPage = groupItemRepository.findAll(
                cmd.pageNumber(),
                cmd.pageSize()
        );

        return new GetGroupShopDetailsResult(
                groupShop.getId(),
                groupShop.getName(),
                groupShop.getDescription(),
                groupItemsPage.map(this::toResult)
        );
    }

    private GetGroupShopDetailsResult.GroupShopItemDto toResult(GroupItem groupItem) {
        return new GetGroupShopDetailsResult.GroupShopItemDto(
                groupItem.getId(),
                groupItem.getName(),
                groupItem.getPrice(),
                groupItem.getIsActive()
        );
    }
}
