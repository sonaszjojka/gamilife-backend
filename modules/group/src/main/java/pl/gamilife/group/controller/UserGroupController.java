package pl.gamilife.group.controller;

import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.group.usecase.findallgroupsbyuseridwhereuserismember.FindAllGroupsByUserIdWhereUserIsMemberCommand;
import pl.gamilife.group.usecase.findallgroupsbyuseridwhereuserismember.FindAllGroupsByUserIdWhereUserIsMemberResult;
import pl.gamilife.group.usecase.findallgroupsbyuseridwhereuserismember.FindAllGroupsByUserIdWhereUserIsMemberUseCase;
import pl.gamilife.shared.web.security.annotation.AuthenticatedUserIsOwner;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/users/{userId}/groups")
@AllArgsConstructor
public class UserGroupController {

    private FindAllGroupsByUserIdWhereUserIsMemberUseCase findAllGroupsByUserIdWhereUserIsMemberUseCase;

    @GetMapping
    @AuthenticatedUserIsOwner
    public ResponseEntity<FindAllGroupsByUserIdWhereUserIsMemberResult> getAllGroupsByUserId(
            @PathVariable UUID userId,

            @RequestParam(required = false) Integer groupType,

            @RequestParam(required = false) String groupName,

            @RequestParam(defaultValue = "0")
            @Min(0) Integer page,

            @RequestParam(defaultValue = "10")
            @Min(1) @Max(100) Integer size

    ) {
        FindAllGroupsByUserIdWhereUserIsMemberResult result = findAllGroupsByUserIdWhereUserIsMemberUseCase.execute(
                new FindAllGroupsByUserIdWhereUserIsMemberCommand(
                        userId, page, size, groupType, groupName
                )
        );
        return ResponseEntity.ok(result);
    }

}
