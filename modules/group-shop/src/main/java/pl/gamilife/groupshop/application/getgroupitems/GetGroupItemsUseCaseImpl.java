package pl.gamilife.groupshop.application.getgroupitems;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.model.filter.GroupItemsFilter;
import pl.gamilife.groupshop.domain.port.repository.GroupItemRepository;
import pl.gamilife.shared.kernel.architecture.Page;

@Service
@AllArgsConstructor
public class GetGroupItemsUseCaseImpl implements GetGroupItemsUseCase {
    private final GroupItemRepository groupItemRepository;


    @Override
    public Page<GetGroupItemResult> execute(GetGroupItemsCommand cmd) {

        Page<GroupItem> groupItemsPage = groupItemRepository.findAll(
                new GroupItemsFilter(cmd.shopId(), cmd.isActive()),
                cmd.pageNumber(),
                cmd.pageSize()
        );

        return groupItemsPage.map(this::toResult);
    }

    private GetGroupItemResult toResult(GroupItem groupItem) {
        return new GetGroupItemResult(
                groupItem.getId(),
                groupItem.getName(),
                groupItem.getPrice(),
                groupItem.getIsActive()
        );
    }
}
