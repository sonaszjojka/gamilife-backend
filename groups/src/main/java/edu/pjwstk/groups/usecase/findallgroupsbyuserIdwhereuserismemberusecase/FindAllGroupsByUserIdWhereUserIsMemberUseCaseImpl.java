package edu.pjwstk.groups.usecase.findallgroupsbyuserIdwhereuserismemberusecase;

import edu.pjwstk.api.groups.dto.FindAllGroupsByUserIdWhereUserIsMemberResult;
import edu.pjwstk.groups.enums.GroupTypeEnum;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.util.UserGroupsSpecificationBuilder;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

@AllArgsConstructor
@Service
@Slf4j
public class FindAllGroupsByUserIdWhereUserIsMemberUseCaseImpl implements FindAllGroupsByUserIdWhereUserIsMemberUseCase {

    private final GroupJpaRepository groupRepository;
    private final UserGroupsSpecificationBuilder specificationBuilder;

    @Override
    public FindAllGroupsByUserIdWhereUserIsMemberResult executeInternal(FindAllGroupsByUserIdWhereUserIsMemberCommand command) {
        GroupTypeEnum groupType = command.groupType() != null
                ? GroupTypeEnum.fromId(command.groupType())
                : null;

        Page<Group> groupPage = groupRepository
                .findAll(
                        getGroupSpecification(command, groupType),
                        createPageable(command)
                );
        return buildFindAllGroupsByUserIdResult(groupPage);
    }

    private FindAllGroupsByUserIdWhereUserIsMemberResult buildFindAllGroupsByUserIdResult(Page<Group> groupPage) {
        return new FindAllGroupsByUserIdWhereUserIsMemberResult(
                groupPage.getTotalPages(),
                groupPage.getTotalPages(),
                groupPage.getNumber(),
                groupPage.getSize(),
                groupPage.getContent().stream().map(g -> new FindAllGroupsByUserIdWhereUserIsMemberResult.GroupDto(
                        g.getGroupId(),
                        g.getJoinCode(),
                        g.getName(),
                        g.getAdminId(),
                        g.getGroupCurrencySymbol(),
                        g.getMembersLimit(),
                        new FindAllGroupsByUserIdWhereUserIsMemberResult.GroupTypeDto(g.getGroupType().getTitle()),
                        g.getGroupMembers().size()
                )).toList()
        );
    }

    private Specification<Group> getGroupSpecification(FindAllGroupsByUserIdWhereUserIsMemberCommand cmd, GroupTypeEnum groupType) {
        return specificationBuilder.buildSpecification(
                cmd.userId(),
                cmd.joinCode(),
                groupType,
                cmd.groupName()
        );
    }

    private Pageable createPageable(FindAllGroupsByUserIdWhereUserIsMemberCommand cmd) {
        return PageRequest.of(
                cmd.page(),
                cmd.size()
        );
    }
}
