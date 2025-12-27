package pl.gamilife.groupshop.application.editgroupshop;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.model.projection.GroupForShop;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;

@Service
@Transactional
@AllArgsConstructor
public class EditGroupShopUseCaseImpl implements EditGroupShopUseCase {
    private final GroupShopRepository groupShopRepository;
    private final GroupContext groupContext;


    @Override
    public EditGroupShopResult execute(EditGroupShopCommand cmd) {

        GroupForShop groupForShop = groupContext.findGroupById(cmd.groupId());
        if (!cmd.userId().equals(groupForShop.adminId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can edit group shop!");
        }

        GroupShop groupShop = groupShopRepository.findByGroupShopId(cmd.groupShopId()).orElseThrow(
                () -> new GroupShopNotFoundException("Group shop with id: " + cmd.groupShopId() + " not found!"));

        groupShop.setName(cmd.name());
        groupShop.setDescription(cmd.description());
        groupShopRepository.save(groupShop);

        return toResult(groupShop);
    }

    private EditGroupShopResult toResult(GroupShop groupShop)
    {
        return new EditGroupShopResult(
                groupShop.getId(),
                groupShop.getGroupId(),
                groupShop.getName(),
                groupShop.getDescription()
        );
    }
}
