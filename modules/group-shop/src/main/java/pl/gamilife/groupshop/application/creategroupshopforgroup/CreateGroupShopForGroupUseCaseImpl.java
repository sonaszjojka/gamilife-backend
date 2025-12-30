package pl.gamilife.groupshop.application.creategroupshopforgroup;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;

@Service
@Transactional
@AllArgsConstructor
public class CreateGroupShopForGroupUseCaseImpl implements CreateGroupShopForGroupUseCase {
    private final GroupShopRepository groupShopRepository;

    @Override
    public CreateGroupShopForGroupResult execute(CreateGroupShopForGroupCommand cmd) {

        GroupShop groupShop = GroupShop.createForGroup(cmd.groupName(), cmd.groupId());
        groupShopRepository.save(groupShop);

        return toResult(groupShop);
    }

    private CreateGroupShopForGroupResult toResult(GroupShop groupShop) {
        return new CreateGroupShopForGroupResult(
                groupShop.getId(),
                groupShop.getGroupId(),
                groupShop.getName(),
                groupShop.getDescription()
        );
    }
}
