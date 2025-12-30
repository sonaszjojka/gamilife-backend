package pl.gamilife.groupshop.application.editgroupshop;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.groupshop.domain.exception.GroupShopNotFoundException;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.port.context.GroupContext;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.GroupMemberNotFoundException;

@Service
@Transactional
@AllArgsConstructor
public class EditGroupShopUseCaseImpl implements EditGroupShopUseCase {
    private final GroupShopRepository groupShopRepository;
    private final GroupContext groupContext;


    @Override
    public EditGroupShopResult execute(EditGroupShopCommand cmd) {
        var member = groupContext.findMemberByUserId(cmd.userId(), cmd.groupId()).orElseThrow(
                () -> new GroupMemberNotFoundException("User is not a member of the group")
        );
        if (!member.isAdmin()) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can edit group shop!");
        }

        GroupShop groupShop = groupShopRepository.findByGroupShopId(cmd.groupId()).orElseThrow(
                () -> new GroupShopNotFoundException("Group shop for group with id: " + cmd.groupId() + " not found!"));

        groupShop.setName(cmd.name());
        groupShop.setDescription(cmd.description());
        groupShopRepository.save(groupShop);

        return toResult(groupShop);
    }

    private EditGroupShopResult toResult(GroupShop groupShop) {
        return new EditGroupShopResult(
                groupShop.getId(),
                groupShop.getGroupId(),
                groupShop.getName(),
                groupShop.getDescription()
        );
    }
}
