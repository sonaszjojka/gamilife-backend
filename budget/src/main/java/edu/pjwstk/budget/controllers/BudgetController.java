package edu.pjwstk.budget.controllers;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/budgets")
public class BudgetController {

    @GetMapping("test")
    public String test() {
        return "Hello World, Budget Controller!";
    }

}
