package pl.gamilife.groupshop.application.getownedgroupitems;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.application.getgroupitems.GetGroupItemResult;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;
import pl.gamilife.groupshop.domain.model.filter.OwnedGroupItemsFilter;
import pl.gamilife.groupshop.domain.port.repository.OwnedGroupItemRepository;
import pl.gamilife.shared.kernel.architecture.Page;

@AllArgsConstructor
@Service
public class GetOwnedGroupItemsUseCaseImpl implements GetOwnedGroupItemsUseCase {
    private final OwnedGroupItemRepository ownedGroupItemRepository;


    @Transactional
    @Override
    public Page<GetOwnedGroupItemsResult> execute(GetOwnedGroupItemsCommand cmd) {

        Page<OwnedGroupItem> ownedGroupItemPage = ownedGroupItemRepository.findAllMemberItems(

                new OwnedGroupItemsFilter(cmd.memberId(), cmd.isUsedUp()),
                cmd.page(),
                cmd.size()
        );


        return ownedGroupItemPage.map(this::toResult);
    }

    private GetOwnedGroupItemsResult toResult(OwnedGroupItem ownedGroupItem) {

        return new GetOwnedGroupItemsResult(
                ownedGroupItem.getId(),
                ownedGroupItem.getGroupMemberId(),
                ownedGroupItem.getUseDate(),
                new GetGroupItemResult(
                        ownedGroupItem.getGroupItem().getId(),
                        ownedGroupItem.getGroupItem().getName(),
                        ownedGroupItem.getGroupItem().getPrice(),
                        ownedGroupItem.getGroupItem().getIsActive()
                )

        );
    }
}
